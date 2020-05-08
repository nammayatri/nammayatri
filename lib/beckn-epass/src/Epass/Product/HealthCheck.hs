module Epass.Product.HealthCheck where

import           Epass.Types.App
import           EulerHS.Prelude

healthCheckApp :: FlowHandler Text
healthCheckApp = pure "App is UP"
