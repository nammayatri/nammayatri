module App.Server where

import App.Routes (appAPI, appServer)
import App.Types
import Beckn.Utils.App
import Beckn.Utils.Monitoring.Prometheus.Metrics
import qualified Beckn.Utils.Servant.Server as BU
import EulerHS.Prelude
import Servant
import Utils.Auth

run :: Env -> Application
run env = \req resp -> do
  modifiedEnv <- modifyEnvR env
  let app =
        addServantInfo appAPI $
          logRequestAndResponse modifiedEnv $
            BU.run appAPI appServer context modifiedEnv
  app req resp
  where
    context =
      verifyApiKey
        :. verifyPersonAction
        :. EmptyContext
