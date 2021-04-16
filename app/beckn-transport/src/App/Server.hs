module App.Server where

import App.Routes (transporterAPI, transporterServer)
import App.Types
import Beckn.Utils.Monitoring.Prometheus.Metrics (addServantInfo)
import qualified Beckn.Utils.Servant.Server as BU
import EulerHS.Prelude
import Servant
import Utils.Auth

run :: Env -> Application
run env =
  addServantInfo transporterAPI $
    BU.run transporterAPI transporterServer context env
  where
    context =
      verifyApiKey
        :. verifyTokenAction
        :. validateAdminAction
        :. validateDriverAction
        :. EmptyContext
