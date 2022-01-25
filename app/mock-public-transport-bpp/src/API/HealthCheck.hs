module API.HealthCheck where

import Beckn.Mock.App
import Beckn.Mock.Environment
import Beckn.Utils.Logging
import Relude

healthCheckServer :: MockM AppEnv Text
healthCheckServer = do
  mockLog DEBUG "got health check request"
  pure "Ferry BPP mock is UP!"
