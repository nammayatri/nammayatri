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
    endpointCapabilities,
    enforce,
    enforceResourceScope,
    enforceResourceScopeFromRequest,
    endpointIdForAction,
  )
where

import Data.List (nub)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Domain.Types.AccessMatrix as DMatrix
import qualified Domain.Types.Capability as DC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ResourceScope as DRS
import qualified Domain.Types.Role as DRole
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Beam.BeamFlow
import qualified Storage.Queries.Capability as QCap
import qualified Storage.Queries.CapabilityEndpoint as QCE
import qualified Storage.Queries.PersonCapability as QPC
import qualified Storage.Queries.PersonResourceAccess as QPRA
import qualified Storage.Queries.PersonTier as QPT
import qualified Storage.Queries.RoleCapability as QRC
import Tools.Error (ResourceScopeError (..))

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

-- | The endpoint_id for a DSL action, from just its UserActionType — what a
-- proxy handler has on its ApiTokenInfo. Same value mkEndpointId produces for
-- DSL entities. Handlers use it to look up the endpoint's resource scoping.
endpointIdForAction :: DMatrix.UserActionType -> Text
endpointIdForAction uat = show (DMatrix.UserActionTypeWrapper uat)

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

-- | The capability_endpoint rows for an endpoint — which capabilities grant it
-- and each one's Layer C binding. verifyApi fetches this ONCE and shares it with
-- both the Layer A (enforce) and Layer C (enforceResourceScopeFromRequest) checks.
endpointCapabilities :: BeamFlow m r => Text -> m [DC.CapabilityEndpoint]
endpointCapabilities = QCE.findAllByEndpointId

enforce ::
  BeamFlow m r =>
  CachedAccess ->
  [DC.CapabilityEndpoint] ->
  DP.Person ->
  Text ->
  m ()
enforce access endpoints person endpointId =
  if access.adminTier == DC.superAdminTier
    then
      logTagError "SUPER_ADMIN_BREAKGLASS" $
        "personId: " <> person.id.getId <> ", endpointId: " <> endpointId
    else do
      -- An endpoint may be assigned to several capabilities; holding ANY of
      -- them grants the call. Same ANY-of rule the frontend applies to
      -- NavItem.requires.
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

------------------------------------------------- resource scope (Layer C)

-- | Path-capture read: the segment(s) immediately following @marker@ in the
-- request's path segments — e.g. marker "specialLocation" on
-- @[…,"specialLocation","sl-42","gates","upsert"]@ → @["sl-42"]@.
pathValueAfter :: Text -> [Text] -> [Text]
pathValueAfter marker segs = [nxt | (s, nxt) <- zip segs (drop 1 segs), s == marker]

-- | The scoped resource types the caller HOLDS for an endpoint, or Nothing when
-- an unscoped held capability lifts the restriction (admin) — the caller then
-- passes unconditionally. `capability.resource_type` is the per-capability switch.
heldScopedTypes :: (BeamFlow m r, Redis.HedisFlow m r) => CachedAccess -> Text -> m (Maybe [DRS.ResourceType])
heldScopedTypes access endpointId = do
  endpointCaps <- QCap.endpointCapabilityTypes endpointId
  let held = filter (\(cid, _) -> cid `elem` access.capabilities) endpointCaps
  logTagInfo "LAYER_C" $ "heldScopedTypes endpointCaps=" <> show endpointCaps <> " held=" <> show held
  pure $ if any (isNothing . snd) held then Nothing else Just (nub (mapMaybe snd held))

-- | Core check: the request's ids for one type must be in the caller's allowlist
-- (DRS.wildcardResourceId "*" = full-MOC; empty allowlist = deny-all). 403 on the
-- first stray id.
checkResourceIds ::
  BeamFlow m r =>
  DP.Person ->
  Text ->
  Id DM.Merchant ->
  City.City ->
  DRS.ResourceType ->
  [Text] ->
  m ()
checkResourceIds person endpointId merchantId city resourceType targetIds = do
  allowed <- QPRA.findResourceIds person.id merchantId city resourceType
  logTagInfo "LAYER_C" $ "checkResourceIds type=" <> show resourceType <> " city=" <> show city <> " merchantId=" <> merchantId.getId <> " allowed=" <> show allowed <> " targets=" <> show targetIds
  let allowedSet = Set.fromList allowed
  unless (DRS.wildcardResourceId `Set.member` allowedSet) $
    case targetIds of
      -- Scoped endpoint but no id resolved from the request: almost always a
      -- binding gap (the id is in the body / under a param the convention doesn't
      -- cover — such endpoints should be marked __HANDLER__ or __SKIP__). We log
      -- LOUDLY but PASS rather than deny: a single capability can back many
      -- endpoints (some id-carrying, some not, some a different resource type),
      -- so failing closed here would break every not-yet-bound sibling. Flip this
      -- to a throw once every endpoint under a scoped capability carries an
      -- explicit binding. See review finding I-security.
      [] ->
        logTagError "RESOURCE_SCOPE_UNRESOLVED" $
          "scoped endpoint resolved zero resource ids — passing (fail-open); set capability_endpoint.resource_id_param (or __SKIP__/__HANDLER__). "
            <> "endpointId: "
            <> endpointId
            <> ", personId: "
            <> person.id.getId
            <> ", resourceType: "
            <> show resourceType
      _ ->
        case filter (`Set.notMember` allowedSet) targetIds of
          [] -> pure ()
          (stray : _) -> do
            logTagError "RESOURCE_OUT_OF_SCOPE" $
              "endpointId: " <> endpointId <> ", personId: " <> person.id.getId
                <> ", roleId: "
                <> person.roleId.getId
                <> ", merchantId: "
                <> merchantId.getId
                <> ", city: "
                <> show city
                <> ", resourceType: "
                <> show resourceType
                <> ", resourceId: "
                <> stray
            throwError (ResourceOutOfScope (show resourceType) stray)

-- | The GENERIC ops gate — run in verifyApi for every endpoint, no per-handler
-- code. Resolves the endpoint's scoping from data (capability.resource_type +
-- capability_endpoint.resource_id_param) and reads the resource id from the
-- request's path captures only ("param:<name>"). SUPER_ADMIN / unscoped-capability
-- holders pass; a "__SKIP__"/"__HANDLER__" binding short-circuits.
enforceResourceScopeFromRequest ::
  BeamFlow m r =>
  CachedAccess ->
  [DC.CapabilityEndpoint] ->
  DP.Person ->
  Text ->
  Id DM.Merchant ->
  City.City ->
  [Text] ->
  m ()
enforceResourceScopeFromRequest access endpoints person endpointId merchantId city pathSegments = do
  logTagInfo "LAYER_C" $ "enter endpointId=" <> endpointId <> " adminTier=" <> access.adminTier <> " capsCount=" <> show (length access.capabilities) <> " pathSegs=[" <> T.intercalate "," pathSegments <> "]"
  unless (access.adminTier == DC.superAdminTier) $ do
    let heldRows = filter (\ce -> ce.capabilityId.getId `elem` access.capabilities) endpoints
        binding = listToMaybe (mapMaybe (.resourceIdParam) heldRows)
    logTagInfo "LAYER_C" $ "binding=" <> show binding
    unless (binding == Just DRS.BindSkip || binding == Just DRS.BindHandler) $ do
      heldCaps <- catMaybes <$> mapM (QCap.findById . (.capabilityId)) heldRows
      let heldTypes = map (.resourceType) heldCaps
          mbTypes = if any isNothing heldTypes then Nothing else Just (nub (catMaybes heldTypes))
      logTagInfo "LAYER_C" $ "mbTypes=" <> show mbTypes
      forM_ mbTypes $ \types ->
        forM_ types $ \resourceType -> do
          let ids = case binding of
                Just (DRS.BindParam name) -> pathValueAfter name pathSegments
                _ -> []
          logTagInfo "LAYER_C" $ "checking type=" <> show resourceType <> " ids=" <> show ids
          checkResourceIds person endpointId merchantId city resourceType ids

-- | The __HANDLER__ fallback: a handler calls this once it has resolved the id(s)
-- the request acts on (e.g. ticket-verify's serviceId → owning place id) — for
-- the minority of endpoints whose resource id isn't a plain query param.
enforceResourceScope ::
  (BeamFlow m r, Redis.HedisFlow m r) =>
  DP.Person ->
  Text ->
  Id DM.Merchant ->
  City.City ->
  [Text] ->
  m ()
enforceResourceScope person endpointId merchantId city targetIds = do
  access <- resolveAccess person.id person.roleId
  unless (access.adminTier == DC.superAdminTier) $ do
    mbTypes <- heldScopedTypes access endpointId
    forM_ mbTypes $ \types ->
      forM_ types $ \resourceType ->
        checkResourceIds person endpointId merchantId city resourceType targetIds
