module External.Gateway.Flow where

import Beckn.Types.API.Search
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Types as API
import Servant.Client
import System.Environment

search ::
  BaseUrl -> SearchReq -> L.Flow (Either Text ())
search url req = do
  res <- L.callAPI url $ API.search req
  whenRight res $ \_ ->
    L.logInfo "Search" "Search successfully delivered"
  whenLeft res $ \err ->
    L.logError "Search" ("error occurred while search: " <> (show err))
  return $ first show res

baseUrl :: BaseUrl
baseUrl =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = "https://api.juspay.in/gateway",
      baseUrlPort = 443,
      baseUrlPath = ""
    }
