module App.Server where

import App.Routes (appAPI, appServer)
import App.Types
import Beckn.Types.App
import Beckn.Utils.Common
import Beckn.Utils.Monitoring.Prometheus.Metrics
import qualified Beckn.Utils.Servant.Server as BU
import qualified Beckn.Utils.Servant.Trail.Handler as Trail
import qualified Beckn.Utils.Servant.Trail.Server as Trail
import EulerHS.Prelude
import Servant
import Utils.Auth
import Utils.Common

run :: Env -> Application
run env =
  addServantInfo appAPI $
    Trail.traceRequests traceHandler $
      BU.run appAPI appServer context env
  where
    context = verifyApiKey :. verifyPersonAction :. EmptyContext
    traceHandler =
      Trail.hoistTraceHandler (runFlowR (runTime env) (appEnv env)) Trail.traceHandler
