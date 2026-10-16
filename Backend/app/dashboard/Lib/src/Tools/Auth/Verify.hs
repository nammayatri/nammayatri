-- | The app-independent half of dashboard API auth: resolve a session token to
-- a person, check their capability for an endpoint, and confirm merchant/city
-- scope. Everything here is keyed by an endpoint id 'Text' rather than by
-- 'UserActionType', which is what lets the application servers link this
-- without depending on the dashboard's API types.
--
-- 'Tools.Auth.ApiAuth' is the Servant-facing wrapper that computes the endpoint
-- id and adds the action type back on.
module Tools.Auth.Verify
  ( VerifiedUser (..),
    verifyDashboardUser,
    verifySession,
    verifyPersonCredentials,
    verifyAccessLevel,
    verifyServerWithPair,
    verifyServer,
    verifyCity,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Role as DRole
import qualified Domain.Types.ServerName as DSN
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant (parseUrlPiece)
import Storage.Beam.BeamFlow
import qualified Storage.Queries.Merchant as QM
import qualified Storage.Queries.MerchantAccess as QAccess
import qualified Storage.Queries.MerchantPair as QMerchantPair
import qualified Storage.Queries.Person as QPerson
import qualified Tools.Auth.Capability as Capability
import qualified Tools.Auth.Common as Common

-- | What the auth path establishes about the caller, minus anything that names
-- the dashboard's API action types.
data VerifiedUser = VerifiedUser
  { personId :: Id DP.Person,
    merchant :: DM.Merchant,
    city :: City.City,
    person :: DP.Person
  }

verifyDashboardUser ::
  ( Common.AuthFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["passwordExpiryDays" ::: Maybe Int]
  ) =>
  DSN.ServerName ->
  Text ->
  -- | Request path segments. Layer C reads the scoped endpoint's resource id
  -- from a @{param}@ capture.
  [Text] ->
  RegToken ->
  m VerifiedUser
verifyDashboardUser requiredServerAccess endpointId pathSegments token = do
  session <- verifySession requiredServerAccess token
  -- One token per person: the token lands on a single merchant/city, but a single
  -- token addresses every MOC the person is provisioned for. Re-scope the session
  -- to the URL's merchant/city when the person holds merchant_access for it, so
  -- Layer C (resource scope), the audit trail and the DashboardUser handed to the
  -- handler all reflect the MOC actually being acted on -- not the token's landing
  -- MOC. Layer B (deny when the person has no access for the URL MOC) is enforced
  -- at handler entry by Tools.Auth.Scope.enforceMerchantCityAccess; the old
  -- verifyUrlScope URL-vs-token check is gone.
  verified <- resolveScopeFromUrl requiredServerAccess session pathSegments
  access <- Capability.resolveAccess verified.person.id verified.person.roleId
  endpoints <- Capability.endpointCapabilities endpointId
  Capability.enforce access endpoints verified.person endpointId -- Layer A -- may this operator call this endpoint at all
  Capability.enforceResourceScopeFromRequest -- Layer C -- and may they touch the specific resource named in the path
    access
    endpoints
    verified.person
    endpointId
    verified.merchant.id
    verified.city
    pathSegments
  pure verified

-- | Authentication only: who is this, and may they act on this server, merchant
-- and city. Deliberately does NOT check a capability, so it can be used where
-- the endpoint id is not yet known -- during cutover, when a route is still
-- authorized upstream by provider-dashboard.
--
-- Anything using this is authenticated but not authorized; pair it with
-- 'Capability.enforce' as 'verifyDashboardUser' does before it becomes the only
-- gate on a mutating endpoint.
verifySession ::
  ( Common.AuthFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["passwordExpiryDays" ::: Maybe Int]
  ) =>
  DSN.ServerName ->
  RegToken ->
  m VerifiedUser
verifySession requiredServerAccess token = do
  (personId, merchantId, city) <- Common.verifyPerson token
  verifiedPerson <- verifyPersonCredentials personId
  verifiedMerchant <- verifyServerWithPair requiredServerAccess personId merchantId city
  verifyCity verifiedMerchant city
  pure VerifiedUser {personId = verifiedPerson.id, merchant = verifiedMerchant, city = city, person = verifiedPerson}

-- | One session, many scopes. A token lands on a single merchant and city (at
-- 'login'), but the permission itself lives in @merchant_access@, one row per
-- person, merchant and city. When the URL names a merchant and city the caller
-- holds such a row for, serve the request under that scope instead of the
-- token's landing scope. This is what lets an operator, or an integrator with
-- one token, address every merchant and city they are provisioned for -- and it
-- is what keeps Layer C (resource scope), the audit trail and the DashboardUser
-- aligned with the MOC actually being acted on, not the MOC the token happened
-- to be issued for.
--
-- Every route reaching here is mounted as
-- @direct-dashboard\/{merchantShortId}\/{city}\/…@ (API.DirectDashboard in
-- rider-app and driver-app), so the scope is read from fixed path positions,
-- parsed exactly as the handler's Capture parses them.
--
-- The guards are the ones 'switchMerchantAndCity' applies: the merchant serves
-- this platform and the person holds access for that exact merchant and city.
-- When either fails the session is returned untouched and the handler-entry
-- 'Tools.Auth.Scope.enforceMerchantCityAccess' denies the call. So this is
-- strictly a re-scope: it never grants access a switch would not already grant.
--
-- Costs nothing when the URL already matches the token; one merchant read and
-- one @merchant_access@ read otherwise.
resolveScopeFromUrl ::
  BeamFlow m r =>
  DSN.ServerName ->
  VerifiedUser ->
  [Text] ->
  m VerifiedUser
resolveScopeFromUrl requiredServerAccess verified pathSegments =
  case pathSegments of
    ("direct-dashboard" : merchantSeg : citySeg : _)
      | Right urlCity <- parseUrlPiece citySeg,
        merchantSeg /= tokenMerchantShortId || urlCity /= verified.city -> do
        mbMerchant <-
          if merchantSeg == tokenMerchantShortId
            then pure (Just verified.merchant)
            else QM.findByShortId (ShortId merchantSeg)
        case mbMerchant of
          Just merchant | requiredServerAccess `elem` merchant.serverNames -> do
            mbAccess <- QAccess.findByPersonIdAndMerchantIdAndCity verified.personId merchant.id urlCity
            pure $ case mbAccess of
              Just _ -> VerifiedUser {personId = verified.personId, merchant = merchant, city = urlCity, person = verified.person}
              Nothing -> verified
          _ -> pure verified
    _ -> pure verified
  where
    tokenMerchantShortId = verified.merchant.shortId.getShortId

-- | Single-login across platforms (dashboard unification): a token is bound
-- to ONE merchant row, but BAP and BPP merchants are separate rows (e.g.
-- NAMMA_YATRI vs NAMMA_YATRI_PARTNER). When the token's merchant cannot serve
-- the required platform, resolve its logical partner via merchant_pair and
-- authorize against that instead — requiring the person to hold
-- merchant_access on the partner for the same city. With no pair row (the
-- pre-merge schemas, or unpaired merchants) this reduces to exactly the
-- legacy verifyServer behavior: AccessDenied.
verifyServerWithPair ::
  BeamFlow m r =>
  DSN.ServerName ->
  Id DP.Person ->
  Id DM.Merchant ->
  City.City ->
  m DM.Merchant
verifyServerWithPair requiredServerAccess personId merchantId city = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  if requiredServerAccess `elem` merchant.serverNames
    then return merchant
    else do
      mbPair <- QMerchantPair.findByMerchantId merchantId
      partnerId <- case mbPair of
        Nothing -> throwError AccessDenied
        Just pair -> do
          let partner
                | (getId <$> pair.bapMerchantId) == Just merchantId.getId = pair.bppMerchantId
                | otherwise = pair.bapMerchantId
          maybe (throwError AccessDenied) pure partner
      partnerMerchant <- QM.findById partnerId >>= fromMaybeM (MerchantNotFound partnerId.getId)
      unless (requiredServerAccess `elem` partnerMerchant.serverNames) $ throwError AccessDenied
      -- The person must be provisioned on the partner side too; token city
      -- scope carries over 1:1.
      void $ QAccess.findByPersonIdAndMerchantIdAndCity personId partnerMerchant.id city >>= fromMaybeM AccessDenied
      return partnerMerchant

verifyServer ::
  BeamFlow m r =>
  DSN.ServerName ->
  Id DM.Merchant ->
  m DM.Merchant
verifyServer requiredServerAccess merchantId = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (requiredServerAccess `elem` merchant.serverNames) $ throwError AccessDenied
  return merchant

verifyCity :: MonadFlow m => DM.Merchant -> City.City -> m ()
verifyCity merchant city = unless (city `elem` merchant.supportedOperatingCities) $ throwError AccessDenied

-- Authorization is capability-only. The access_matrix is not consulted here
-- any more: `Tools.Auth.Capability.enforce` is the entire verdict, and an
-- endpoint with no capability behind it is denied rather than waved through.
-- `0017-capability-backfill.sql` is the data migration that makes the
-- capability set a superset of the old matrix, and must have run before this
-- code is deployed.
verifyAccessLevel ::
  ( BeamFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["passwordExpiryDays" ::: Maybe Int]
  ) =>
  Text ->
  Id DP.Person ->
  m DP.Person
verifyAccessLevel endpointId personId = do
  person <- verifyPersonCredentials personId
  -- Layer A only. This is a handler-level RBAC check with no request in scope,
  -- so there is no path to read a resource id from; Layer C is enforced on the
  -- servant auth path in 'verifyDashboardUser'.
  access <- Capability.resolveAccess person.id person.roleId
  endpoints <- Capability.endpointCapabilities endpointId
  Capability.enforce access endpoints person endpointId
  pure person

-- | The password/enablement checks that gate any session, independent of which
-- endpoint is being called.
verifyPersonCredentials ::
  ( BeamFlow m r,
    HasFlowEnv m r '["passwordExpiryDays" ::: Maybe Int]
  ) =>
  Id DP.Person ->
  m DP.Person
verifyPersonCredentials personId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  -- Unconditional: an admin-assigned password must not authorize anything at any tier.
  Common.checkForcedPasswordChange person
  maybe (pure ()) (\a -> when (a `elem` [DRole.DASHBOARD_ADMIN, DRole.DASHBOARD_USER]) $ Common.checkPasswordExpiry person) person.dashboardAccessType
  pure person
