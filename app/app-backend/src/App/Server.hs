module App.Server where

import App.Routes (appAPI, appServer)
import App.Types
import Beckn.Utils.Monitoring.Prometheus.Metrics
import qualified Beckn.Utils.Servant.Server as BU
import qualified Data.Vault.Lazy as V
import EulerHS.Prelude
import Servant
import Utils.Auth
import Utils.Common

run :: V.Key (HashMap Text Text) -> Env -> Application
run key env = addServantInfo appAPI $ BU.run appAPI (appServer key) context env
  where
    context = verifyApiKey :. verifyPersonAction :. EmptyContext
