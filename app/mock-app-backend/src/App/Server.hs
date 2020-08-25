module App.Server
  ( run,
  )
where

import App.Routes (mockAppBackendAPI, mockAppBackendServer)
import App.Types
import Beckn.Utils.Monitoring.Prometheus.Metrics
import qualified Beckn.Utils.Servant.Server as BU
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import Servant
import Utils.Auth

run :: V.Key (HashMap Text Text) -> Env -> Application
run key env =
  addServantInfo mockAppBackendAPI $
    BU.run mockAppBackendAPI (mockAppBackendServer key) context env
  where
    context = verifyApiKey :. EmptyContext
