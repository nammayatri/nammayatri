module App.Server where

import App.Routes (transporterAPI, transporterServer)
import App.Types
import Beckn.Types.App
import Beckn.Utils.Common
import Beckn.Utils.Monitoring.Prometheus.Metrics (addServantInfo)
import qualified Beckn.Utils.Servant.Server as BU
import qualified Beckn.Utils.Servant.Trail.Handler as Trail
import qualified Beckn.Utils.Servant.Trail.Server as Trail
import EulerHS.Prelude
import Servant
import Utils.Auth
import Utils.Common

run :: Env -> Application
run env =
  addServantInfo transporterAPI $
    Trail.traceRequests traceHandler $
      BU.run transporterAPI transporterServer context env
  where
    context =
      verifyApiKey
        :. verifyTokenAction
        :. verifyOrgAction
        :. validateAdminAction
        :. validateDriverAction
        :. EmptyContext
    traceHandler =
      Trail.hoistTraceHandler (runFlowR (runTime env) (appEnv env)) Trail.traceHandler
