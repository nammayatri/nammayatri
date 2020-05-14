module External.Gateway.Flow where

import Beckn.Types.API.Search
import Beckn.Types.API.Track
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.API as API
import External.Gateway.Types
import Servant.Client
import System.Environment

onSearch ::
  BaseUrl -> OnSearchReq -> L.Flow (Either Text ())
onSearch url req = do
  res <- L.callAPI url $ API.onSearch req
  whenRight res $ \_ ->
    L.logInfo "OnSearch" $ "OnSearch callback successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending onSearch Callback: " (show err)
  return $ first show res

onTrackTrip ::
  BaseUrl -> OnTrackTripReq -> L.Flow (Either Text ())
onTrackTrip url req = do
  res <- L.callAPI url $ API.onTrackTrip req
  whenRight res $ \_ ->
    L.logInfo "OnTrackTrip" $ "OnTrackTrip callback successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending OnTrackTrip Callback: " (show err)
  return $ first show res

defaultBaseUrl :: String -> BaseUrl
defaultBaseUrl baseUrl = do
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = baseUrl,
      baseUrlPort = 8013,
      baseUrlPath = "/v1"
    }
