module App.Server where

import App.Routes (transporterAPI, transporterServer)
import App.Types
import Beckn.Utils.Monitoring.Prometheus.Metrics (addServantInfo)
import qualified Beckn.Utils.Servant.Server as BU
import qualified Beckn.Utils.Servant.Trail.Server as Trail
import EulerHS.Prelude
import Servant
import Utils.Auth
import Utils.Common

run :: Env -> Application
run env =
  addServantInfo transporterAPI $
    Trail.toTraceOrNotToTrace env $
      BU.run transporterAPI transporterServer context env
  where
    context =
      verifyApiKey
        :. verifyTokenAction
        :. validateAdminAction
        :. validateDriverAction
        :. EmptyContext
