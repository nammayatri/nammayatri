module Fixtures.BaseUrl where

import Servant.Client

defaultUrl :: BaseUrl
defaultUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "api.example.com",
      baseUrlPort = 80,
      baseUrlPath = ""
    }