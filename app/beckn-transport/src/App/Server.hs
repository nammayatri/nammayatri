module App.Server where

import App.Routes (transporterAPI, transporterServer)
import App.Types
import Beckn.Types.Flow
import Beckn.Utils.App
import Beckn.Utils.Monitoring.Prometheus.Metrics (addServantInfo)
import qualified Beckn.Utils.Servant.Server as BU
import EulerHS.Prelude
import Servant
import Utils.Auth

run :: Env -> Application
run = withModifiedEnv $ \modifiedEnv ->
  BU.run transporterAPI transporterServer context modifiedEnv
    & logRequestAndResponse modifiedEnv
    & addServantInfo transporterAPI
    & hashBodyForSignature
    & supportProxyAuthorization
  where
    context =
      verifyTokenAction @(FlowR AppEnv)
        :. validateAdminAction @(FlowR AppEnv)
        :. EmptyContext
