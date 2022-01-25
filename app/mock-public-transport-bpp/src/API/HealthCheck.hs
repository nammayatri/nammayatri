module API.HealthCheck where

import Beckn.Utils.Logging
import Common.App
import Common.Environment
import Relude

healthCheckServer :: MockM AppEnv Text
healthCheckServer = do
  mockLog DEBUG "got health check request"
  pure "Ferry BPP mock is UP!"
