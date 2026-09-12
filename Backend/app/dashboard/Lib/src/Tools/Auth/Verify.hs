{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | The app-independent half of dashboard API auth: resolve a session token to
-- a person, check their capability for an endpoint, and confirm merchant/city
-- scope. Everything here is keyed by an endpoint id 'Text' rather than by
-- 'UserActionType', which is what lets the application servers link this
-- without depending on the dashboard's API types.
--
-- 'Tools.Auth.Api' in lib-dashboard-api is the Servant-facing wrapper that
-- computes the endpoint id and adds the action type back on.
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
  -- from a @{param}@ capture, so it needs the raw path. Supplied by
  -- 'Tools.Servant.HeaderAuth' from @rawPathInfo@.
  [Text] ->
  RegToken ->
  m VerifiedUser
verifyDashboardUser requiredServerAccess endpointId pathSegments token = do
  verified <- verifySession requiredServerAccess token
  -- Authorization. Resolve the operator's access and this endpoint's capability
  -- rows ONCE, then run both gates off the same data:
  --   Layer A -- may this operator call this endpoint at all
  --   Layer C -- and may they touch the specific resource named in the path
  -- Both must run here, or a route served directly would be authorized more
  -- loosely than the same route served through provider-dashboard.
  access <- Capability.resolveAccess verified.person.id verified.person.roleId
  endpoints <- Capability.endpointCapabilities endpointId
  Capability.enforce access endpoints verified.person endpointId
  Capability.enforceResourceScopeFromRequest
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
