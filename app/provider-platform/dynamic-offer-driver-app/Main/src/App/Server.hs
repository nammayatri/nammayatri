module App.Server where

import API
import Beckn.Core (logBecknRequest)
import Environment
import EulerHS.Prelude
import Kernel.Tools.Metrics.Init
import Kernel.Types.Flow
import Kernel.Utils.App
import Kernel.Utils.Monitoring.Prometheus.Servant ()
import qualified Kernel.Utils.Servant.Server as BU
import Servant
import Tools.Auth

run :: Env -> Application
run = withModifiedEnv $ \modifiedEnv ->
  BU.run driverOfferAPI driverOfferServer context modifiedEnv
    & logRequestAndResponse modifiedEnv
    & logBecknRequest modifiedEnv.appEnv
    & addServantInfo driverOfferAPI
    & hashBodyForSignature
    & supportProxyAuthorization
  where
    context =
      verifyTokenAction @(FlowR AppEnv)
        :. validateAdminAction @(FlowR AppEnv)
        :. verifyDashboardAction @(FlowR AppEnv)
        :. EmptyContext
