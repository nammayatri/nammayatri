module Beckn.App.Routes where

import Beckn.Types.App
import EulerHS.Prelude
import Servant

type EPassAPIs = "api" :> "v1" :> Get '[ JSON] Text

epassAPIs :: Proxy EPassAPIs
epassAPIs = Proxy

healthCheckApp :: FlowHandler Text
healthCheckApp = pure "App is UP"
