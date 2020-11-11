module App.Server
  ( run,
  )
where

import App.Routes (mockAppBackendAPI, mockAppBackendServer)
import App.Types
import Beckn.Utils.Monitoring.Prometheus.Metrics
import qualified Beckn.Utils.Servant.Server as BU
import EulerHS.Prelude
import Servant
import Utils.Auth

run :: Env -> Application
run env =
  addServantInfo mockAppBackendAPI $
    BU.run mockAppBackendAPI mockAppBackendServer context env
  where
    context = verifyApiKey :. EmptyContext
