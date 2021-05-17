module App.BackgroundTaskManager.Routes where

import App.Types
import EulerHS.Prelude
import qualified Product.HealthCheck as HealthCheck
import Servant

type HealthCheckAPI = Get '[JSON] Text

healthCheckServer :: FlowServer HealthCheckAPI
healthCheckServer = HealthCheck.healthCheck

healthCheckAPI :: Proxy HealthCheckAPI
healthCheckAPI = Proxy
