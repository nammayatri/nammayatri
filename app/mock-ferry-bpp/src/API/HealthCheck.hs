module API.HealthCheck where

import Beckn.Utils.Logging
import Common.App
import Relude

healthCheckServer :: MockM Text
healthCheckServer = do
  mockLog DEBUG "got health check request"
  pure "Ferry BPP mock is UP!"
