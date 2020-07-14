module Epass.Product.HealthCheck where

import App.Types
import Epass.Types.App
import EulerHS.Prelude

healthCheckApp :: FlowHandler Text
healthCheckApp = pure "App is UP"
