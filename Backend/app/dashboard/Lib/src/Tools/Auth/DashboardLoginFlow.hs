{-# LANGUAGE UndecidableSuperClasses #-}

-- | The environment a server needs to run the dashboard's own login, 2FA and
-- user-administration handlers.
--
-- These handlers are deliberately environment-agnostic: they name what they need
-- through 'HasFlowEnv' rather than reaching into a concrete AppEnv. That is what
-- lets provider-dashboard and the application servers all serve them.
--
-- The union is gathered from the constraint lists on
-- @Domain.Action.Dashboard.*@. If a handler there gains a new @HasFlowEnv@, add
-- the matching field to every AppCfg/AppEnv that mounts this tree, or the mount
-- stops compiling -- which is the intended failure mode.
module Tools.Auth.DashboardLoginFlow
  ( DashboardLoginFlow,
    withDashboardDbFlowHandlerAPI,
    dashboardLoginVerifyAction,
  )
where

import qualified Data.HashMap.Strict as HM
import Domain.Types.ServerName (DataServer)
import qualified EulerHS.Language as L
import Kernel.Beam.Functions (runInDashboardDb)
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Flow (FlowR)
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions)
import Kernel.Utils.Common
import Storage.Beam.BeamFlow (BeamFlow)
import qualified Tools.Auth.Common as Common
import Tools.Auth.Dashboard (VerifyDashboard, verifyDashboardAction)
import Tools.Servant.HeaderAuth (VerificationActionWithPayload (..))

type DashboardLoginFlow m r =
  ( BeamFlow m r,
    Common.AuthFlow m r,
    Redis.HedisFlow m r,
    EncFlow m r,
    CoreMetrics m,
    MonadFlow m,
    Log m,
    -- Required by withFlowHandlerAPI', which every handler in this tree wraps with.
    HasField "isShuttingDown" r (TMVar ()),
    HasField "url" r (Maybe Text),
    HasFlowEnv m r '["authTokenCacheKeyPrefix" ::: Text],
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    HasFlowEnv m r '["enforceStrongPasswordPolicy" ::: Bool],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["is2faMandatory" ::: Bool],
    HasFlowEnv m r '["loginRateLimitOptions" ::: APIRateLimitOptions],
    HasFlowEnv m r '["merchantUserAccountNumber" ::: Int],
    HasFlowEnv m r '["passwordExpiryDays" ::: Maybe Int],
    HasFlowEnv m r '["totpClockSkew" ::: Maybe Int],
    HasFlowEnv m r '["totpStepSize" ::: Maybe Int],
    HasFlowEnv m r '["twoFaEnforcementDeadline" ::: Maybe UTCTime],
    HasFlowEnv m r '["twoFaExemptRoles" ::: [Text]],
    HasFlowEnv m r '["twoFaIssuerName" ::: Text],
    HasFlowEnv m r '["twoFaMaxOtpVerifyAttempts" ::: Maybe Int],
    HasFlowEnv m r '["twoFaOtpTTLInSecs" ::: Maybe Int],
    HasFlowEnv m r '["updateRestrictedBppRoles" ::: [Text]]
  )

-- | Run a login-tree handler with its queries pointed at the dashboard database.
--
-- provider-dashboard's own database /is/ the dashboard database, so there this
-- must not switch anything. An application server has its own database and
-- reaches the dashboard one only inside 'runInDashboardDb'. Whether a dashboard
-- database is registered tells the two apart, so handlers stay identical on
-- both servers and neither has to know where it is running.
-- Named for the database it switches, not to shadow shared-kernel's
-- 'withDashboardFlowHandlerAPI', which is a different wrapper and is re-exported
-- by Kernel.Utils.Common.
withDashboardDbFlowHandlerAPI ::
  ( CoreMetrics (FlowR r),
    Log (FlowR r),
    HasField "isShuttingDown" r (TMVar ()),
    HasField "url" r (Maybe Text)
  ) =>
  FlowR r a ->
  FlowHandlerR r a
withDashboardDbFlowHandlerAPI = withFlowHandlerAPI' . inDashboardDbIfConfigured

inDashboardDbIfConfigured :: (L.MonadFlow m, MonadMask m) => m a -> m a
inDashboardDbIfConfigured m = do
  mbDashboardDbCfg <- L.getOption KBT.PsqlDashboardDbCfg
  maybe m (const $ runInDashboardDb m) mbDashboardDbCfg

-- | 'verifyDashboardAction' with its session lookup pointed at the dashboard
-- database.
--
-- An application server serving the login tree must resolve the operator's token
-- against @atlas_dashboard@, not its own database. provider-dashboard registers
-- no dashboard database, so there this is 'verifyDashboardAction' unchanged and
-- both servers can share one context entry.
dashboardLoginVerifyAction ::
  ( Common.AuthFlow m r,
    Redis.HedisFlow m r,
    HasFlowEnv m r '["passwordExpiryDays" ::: Maybe Int],
    L.MonadFlow m,
    MonadMask m
  ) =>
  VerificationActionWithPayload VerifyDashboard m
dashboardLoginVerifyAction =
  case verifyDashboardAction of
    VerificationActionWithPayload verify ->
      VerificationActionWithPayload $ \payload pathSegments token ->
        inDashboardDbIfConfigured (verify payload pathSegments token)
