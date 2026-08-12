{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Tools.Auth.Capability
  ( CachedAccess (..),
    mkEndpointId,
    computeEffective,
    resolveAccess,
    adminTierOf,
    isSuperAdmin,
    invalidatePerson,
    invalidateEveryone,
    enforce,
  )
where

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Capability as DC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Role as DRole
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.BeamFlow
import qualified Storage.Queries.CapabilityEndpoint as QCE
import qualified Storage.Queries.PersonCapability as QPC
import qualified Storage.Queries.PersonTier as QPT
import qualified Storage.Queries.RoleCapability as QRC

-- Capability enforcement — THE authority for dashboard API authorization.
--
-- There is no access_matrix fallback: an endpoint with no capability behind it
-- is denied, not waved through. `0017-capability-backfill.sql` is what makes
-- that safe; it grants every role the capability behind each endpoint the
-- matrix already allowed it, so the capability set is a superset of legacy
-- before this code is deployed. Its section-1 report must be empty (no matrix
-- grant lacking a capability) or those endpoints become unreachable.
--
-- SUPER_ADMIN bypasses the check entirely. That is break-glass by design
-- (PLAN.md admin tiering) — the tier is DB-seeded, never mintable over the
-- API — and every use of it logs.

-- | The endpoint_id this request's action is registered under in
-- capability_endpoint. DSL actions serialize to MODULE/RESOURCE/ACTION via the
-- UserActionTypeWrapper Show instance; legacy (non-DSL) actions carry a
-- LEGACY/<entity>/<action> qualified id.
mkEndpointId :: DMatrix.ApiAccessLevel -> Text
mkEndpointId lvl = do
  let actionStr = show (DMatrix.UserActionTypeWrapper lvl.userActionType)
  case lvl.apiEntity of
    DMatrix.DSL -> actionStr
    entity -> "LEGACY/" <> show entity <> "/" <> actionStr

-- | Everything the auth path needs about a person, cached as one value so a
-- request costs one Redis read instead of three Postgres queries.
data CachedAccess = CachedAccess
  { adminTier :: Text,
    capabilities :: [Text]
  }
  deriving (Show, Generic, FromJSON, ToJSON)

------------------------------------------------------------------- caching

-- Cache invalidation has two shapes. A person_capability change touches one
-- person, so it deletes that person's key. A role_capability change touches
-- every member of the role, and enumerating them is a scan — so instead a
-- global version counter is part of the key, and bumping it retires every
-- cached set at once. The version key outlives the entry TTL by a wide margin,
-- so a version rollback can never resurrect a live stale entry.
capabilityVersionKey :: Text
capabilityVersionKey = "dsh:caps:version"

capabilityCacheTtl :: Int
capabilityCacheTtl = 600

capabilityVersionTtl :: Int
capabilityVersionTtl = 2592000 -- 30 days

capabilityCacheKey :: Int -> Id DP.Person -> Text
capabilityCacheKey version personId =
  "dsh:caps:v" <> show version <> ":" <> personId.getId

currentVersion :: Redis.HedisFlow m r => m Int
currentVersion = fromMaybe 0 <$> Redis.get capabilityVersionKey

-- | Drop one person's cached set — for person_capability grants and denies.
invalidatePerson :: Redis.HedisFlow m r => Id DP.Person -> m ()
invalidatePerson personId = do
  version <- currentVersion
  Redis.del $ capabilityCacheKey version personId

-- | Retire every cached set — for role_capability changes.
invalidateEveryone :: Redis.HedisFlow m r => m ()
invalidateEveryone = do
  version <- currentVersion
  Redis.setExp capabilityVersionKey (version + 1) capabilityVersionTtl

--------------------------------------------------------------- resolution

-- | Effective set = (role capabilities ∪ live GRANTs) − live DENYs.
computeEffective :: BeamFlow m r => Id DP.Person -> Id DRole.Role -> m [Text]
computeEffective personId roleId = do
  now <- getCurrentTime
  roleCaps <- QRC.findAllByRoleId roleId
  overrides <- QPC.findAllByPersonId personId
  let live pc = maybe True (> now) pc.expiresAt
      grants = [pc.capabilityId.getId | pc <- overrides, pc.mode == DC.GRANT, live pc]
      denies = Set.fromList [pc.capabilityId.getId | pc <- overrides, pc.mode == DC.DENY, live pc]
      base = map (.capabilityId.getId) roleCaps
  pure $ Set.toList $ Set.fromList (base <> grants) `Set.difference` denies

adminTierOf :: BeamFlow m r => Id DP.Person -> m Text
adminTierOf personId = maybe DC.userTier (.adminTier) <$> QPT.findByPersonId personId

isSuperAdmin :: BeamFlow m r => Id DP.Person -> m Bool
isSuperAdmin personId = (== DC.superAdminTier) <$> adminTierOf personId

-- | Cached tier + effective capability set.
resolveAccess ::
  (BeamFlow m r, Redis.HedisFlow m r) =>
  Id DP.Person ->
  Id DRole.Role ->
  m CachedAccess
resolveAccess personId roleId = do
  version <- currentVersion
  let key = capabilityCacheKey version personId
  mbCached <- Redis.get key
  case mbCached of
    Just cached -> pure cached
    Nothing -> do
      tier <- adminTierOf personId
      caps <- computeEffective personId roleId
      let value = CachedAccess {adminTier = tier, capabilities = caps}
      Redis.setExp key value capabilityCacheTtl
      pure value

-------------------------------------------------------------- enforcement

-- | Authorize a request, or throw AccessDenied. No legacy fallback.
enforce ::
  (BeamFlow m r, Redis.HedisFlow m r) =>
  DP.Person ->
  Text ->
  m ()
enforce person endpointId = do
  access <- resolveAccess person.id person.roleId
  if access.adminTier == DC.superAdminTier
    then
      logTagError "SUPER_ADMIN_BREAKGLASS" $
        "personId: " <> person.id.getId <> ", endpointId: " <> endpointId
    else do
      -- An endpoint may be assigned to several capabilities; holding ANY of
      -- them grants the call. Same ANY-of rule the frontend applies to
      -- NavItem.requires.
      endpoints <- QCE.findAllByEndpointId endpointId
      let capabilityIds = map (.capabilityId.getId) endpoints
      case capabilityIds of
        -- Fail closed. An unmapped endpoint is a seeding bug, not an open
        -- door: extend the shim in generate_capability_seed.py and reseed.
        [] -> do
          logTagError "CAPABILITY_UNMAPPED_ENDPOINT" $
            "endpointId: " <> endpointId <> ", personId: " <> person.id.getId
          throwError AccessDenied
        _ ->
          unless (any (`elem` access.capabilities) capabilityIds) $ do
            logTagError "CAPABILITY_DENIED" $
              "endpointId: " <> endpointId
                <> ", capabilityIds: "
                <> T.intercalate "|" capabilityIds
                <> ", personId: "
                <> person.id.getId
                <> ", roleId: "
                <> person.roleId.getId
            throwError AccessDenied
