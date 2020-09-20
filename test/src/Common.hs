module Common where

import Servant.Client

gatewayBaseUrl :: BaseUrl
gatewayBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8015,
      baseUrlPath = "/v1"
    }
