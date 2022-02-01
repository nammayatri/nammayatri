module API.HealthCheck where

import Beckn.Mock.App
import Beckn.Utils.Logging
import Environment
import Relude

healthCheckServer :: MockM AppEnv Text
healthCheckServer = do
  logOutput DEBUG "got health check request"
  pure "Ferry BPP mock is UP!"
