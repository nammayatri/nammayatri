module Beckn.Product.HealthCheck where

import           Beckn.Types.App
import           EulerHS.Prelude

healthCheckApp :: FlowHandler Text
healthCheckApp = pure "App is UP"
