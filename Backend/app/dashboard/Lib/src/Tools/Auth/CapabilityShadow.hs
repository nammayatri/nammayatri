{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Auth.CapabilityShadow where

import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Capability as DC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Role as DRole
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.BeamFlow
import qualified Storage.Queries.CapabilityEndpoint as QCE
import qualified Storage.Queries.PersonCapability as QPC
import qualified Storage.Queries.RoleCapability as QRC

-- Shadow-mode capability check (dashboard unification Phase 3). Computes the
-- capability-framework verdict for the current request and logs disagreements
-- with the legacy access_matrix verdict. MUST NEVER affect the request
-- outcome: all failures are swallowed and logged. The enforcement flip
-- (Phase 4) replaces the access_matrix check with this logic once shadow
-- logs show zero mismatches over the bake window.
--
-- TODO(capability): before the Phase 4 flip, cache the effective capability
-- set per person in Redis, invalidated via a capabilityVersion counter bumped
-- on any role_capability/person_capability mutation.

-- | The endpoint_id under which this request's action is registered in
-- capability_endpoint. DSL actions serialize to MODULE/RESOURCE/ACTION via
-- the UserActionTypeWrapper Show instance; legacy (non-DSL) actions get a
-- LEGACY/<entity>/<action> qualified id (only the SPECIAL_ZONES four are
-- seeded; anything else logs as unmapped).
mkShadowEndpointId :: DMatrix.ApiAccessLevel -> Text
mkShadowEndpointId lvl = do
  let actionStr = show (DMatrix.UserActionTypeWrapper lvl.userActionType)
  case lvl.apiEntity of
    DMatrix.DSL -> actionStr
    entity -> "LEGACY/" <> show entity <> "/" <> actionStr

shadowCheck ::
  BeamFlow m r =>
  Id DP.Person ->
  Id DRole.Role ->
  Text ->
  Bool ->
  m ()
shadowCheck personId roleId endpointId legacyAllowed = do
  res <- try @_ @SomeException $ do
    mbEndpoint <- QCE.findByEndpointId endpointId
    case mbEndpoint of
      Nothing ->
        logTagError "CAPABILITY_SHADOW_UNMAPPED" $
          "endpointId: " <> endpointId <> ", personId: " <> personId.getId
      Just endpoint -> do
        now <- getCurrentTime
        roleCaps <- QRC.findAllByRoleId roleId
        overrides <- QPC.findAllByPersonId personId
        let live pc = maybe True (> now) pc.expiresAt
            matches pc = pc.capabilityId == endpoint.capabilityId && live pc
            granted = any (\pc -> matches pc && pc.mode == DC.GRANT) overrides
            denied = any (\pc -> matches pc && pc.mode == DC.DENY) overrides
            fromRole = any (\rc -> rc.capabilityId == endpoint.capabilityId) roleCaps
            capabilityAllowed = (fromRole || granted) && not denied
        -- Direction matters at flip time: the flip verdict is
        -- (capability OR legacy) while the matrix survives as a transitional
        -- fallback, so FALLBACK_NEEDED costs nobody access — it is the
        -- tightening worklist. WIDENING is the reviewable direction.
        let direction :: Text
            direction
              | capabilityAllowed && not legacyAllowed = "WIDENING"
              | otherwise = "FALLBACK_NEEDED"
        when (capabilityAllowed /= legacyAllowed) $
          logTagError "CAPABILITY_SHADOW_MISMATCH" $
            "direction: " <> direction <> ", "
              <> "endpointId: "
              <> endpointId
              <> ", capabilityId: "
              <> endpoint.capabilityId.getId
              <> ", personId: "
              <> personId.getId
              <> ", roleId: "
              <> roleId.getId
              <> ", legacy: "
              <> show legacyAllowed
              <> ", capability: "
              <> show capabilityAllowed
  case res of
    Left err -> logTagError "CAPABILITY_SHADOW_ERROR" $ "endpointId: " <> endpointId <> ", error: " <> show err
    Right () -> pure ()
