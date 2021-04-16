module App.Server where

import App.Routes (appAPI, appServer)
import App.Types
import Beckn.Utils.Monitoring.Prometheus.Metrics
import qualified Beckn.Utils.Servant.Server as BU
import EulerHS.Prelude
import Servant
import Utils.Auth

run :: Env -> Application
run env =
  addServantInfo appAPI $
    BU.run appAPI appServer context env
  where
    context =
      verifyApiKey
        :. verifyPersonAction
        :. EmptyContext
