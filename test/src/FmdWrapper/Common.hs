module FmdWrapper.Common where

import Network.Wai.Handler.Warp
import Servant.Client

fmdTestAppPort :: Port
fmdTestAppPort = 8019

fmdTestAppBaseUrl :: BaseUrl
fmdTestAppBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = fmdTestAppPort,
      baseUrlPath = "/v1"
    }
