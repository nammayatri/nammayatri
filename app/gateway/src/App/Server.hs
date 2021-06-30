module App.Server
  ( run,
  )
where

import App.Routes (gatewayAPI, gatewayServer)
import App.Types
import Beckn.Types.App
import Beckn.Utils.App (hashBodyForSignature)
import Beckn.Utils.Monitoring.Prometheus.Metrics (addServantInfo)
import qualified Beckn.Utils.Servant.Server as BU
import EulerHS.Prelude
import Servant

run :: EnvR AppEnv -> Application
run env =
  BU.run gatewayAPI gatewayServer context env
    & addServantInfo gatewayAPI
    & hashBodyForSignature
  where
    context = EmptyContext
