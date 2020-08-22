module App.Server where

import App.Routes (appAPI, appServer)
import App.Types
import Beckn.Utils.Monitoring.Prometheus.Metrics
import qualified Beckn.Utils.Servant.Server as BU
import qualified Beckn.Utils.Servant.Trail.Server as Trail
import EulerHS.Prelude
import Servant
import Utils.Auth
import Utils.Common

run :: Env -> Application
run env =
  addServantInfo appAPI $
    Trail.toTraceOrNotToTrace env $
      BU.run appAPI appServer context env
  where
    context = verifyApiKey :. verifyPersonAction :. EmptyContext
