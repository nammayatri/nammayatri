module API.HealthCheck where

import Beckn.Prelude
import Beckn.Utils.Logging
import Common.App

healthCheckServer :: MockM Text
healthCheckServer = do
  mockLog DEBUG "got health check request"
  pure "Ferry BPP mock is UP!"
