module App.Server where

import App.Routes (transporterAPI, transporterServer)
import App.Types
import Beckn.Utils.App
import Beckn.Utils.Monitoring.Prometheus.Metrics (addServantInfo)
import qualified Beckn.Utils.Servant.Server as BU
import EulerHS.Prelude
import Servant
import Utils.Auth

run :: Env -> Application
run env = \req resp -> do
  modifiedEnv <- modifyEnvR env
  let app =
        addServantInfo transporterAPI $
          logRequestAndResponse modifiedEnv $
            BU.run transporterAPI transporterServer context modifiedEnv
  app req resp
  where
    context =
      verifyApiKey
        :. verifyTokenAction
        :. validateAdminAction
        :. validateDriverAction
        :. EmptyContext
