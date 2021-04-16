module App.Server
  ( run,
  )
where

import App.Routes (gatewayAPI, gatewayServer)
import App.Types
import Beckn.Types.App
import Beckn.Utils.Monitoring.Prometheus.Metrics (addServantInfo)
import qualified Beckn.Utils.Servant.Server as BU
import qualified Beckn.Utils.Servant.Trail.Server as Trail
import EulerHS.Prelude
import Servant
import Utils.Auth

run :: EnvR AppEnv -> Application
run env = do
  addServantInfo gatewayAPI $
    Trail.toTraceOrNotToTrace env $
      BU.run gatewayAPI gatewayServer context env
  where
    context = verifyAPIKeyAction :. EmptyContext
