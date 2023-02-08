module App.Server where

import API (transporterAPI, transporterServer)
import Core.Beckn (logBecknRequest)
import Environment
import EulerHS.Prelude
import Kernel.Tools.Metrics.Init
import Kernel.Types.Flow
import Kernel.Utils.App
import qualified Kernel.Utils.Servant.Server as BU
import Servant
import Tools.Auth

run :: Env -> Application
run = withModifiedEnv $ \modifiedEnv ->
  BU.run transporterAPI transporterServer context modifiedEnv
    & logRequestAndResponse modifiedEnv
    & logBecknRequest modifiedEnv.appEnv
    & addServantInfo transporterAPI
    & hashBodyForSignature
    & supportProxyAuthorization
  where
    context =
      verifyTokenAction @(FlowR AppEnv)
        :. validateAdminAction @(FlowR AppEnv)
        :. verifyDashboardAction @(FlowR AppEnv)
        :. EmptyContext
