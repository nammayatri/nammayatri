module External.Gateway.Flow where

import Beckn.Types.API.Confirm
import Beckn.Types.API.Search
import Beckn.Types.API.Status
import Beckn.Types.API.Track
import Beckn.Types.API.Cancel
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.API as API
import External.Gateway.Types
import Servant.Client
import System.Environment

onSearch :: OnSearchReq -> L.Flow (Either Text ())
onSearch req = do
  url <- getBaseUrl
  res <- L.callAPI url $ API.onSearch req
  whenRight res $ \_ ->
    L.logInfo "OnSearch" $ "OnSearch callback successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending onSearch Callback: " (show err)
  return $ first show res

onTrackTrip :: OnTrackTripReq -> L.Flow (Either Text ())
onTrackTrip req = do
  url <- getBaseUrl
  res <- L.callAPI url $ API.onTrackTrip req
  whenRight res $ \_ ->
    L.logInfo "OnTrackTrip" $ "OnTrackTrip callback successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending OnTrackTrip Callback: " (show err)
  return $ first show res

onConfirm :: OnConfirmReq -> L.Flow (Either Text ())
onConfirm req = do
  url <- getBaseUrl
  res <- L.callAPI url $ API.onConfirm req
  whenRight res $ \_ ->
    L.logInfo "OnConfirm" $ "OnConfirm callback successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending onConfirm Callback: " (show err)
  return $ first show res

onCancel :: OnCancelReq -> L.Flow (Either Text ())
onCancel req = do
  url <- getBaseUrl
  res <- L.callAPI url $ API.onCancel req
  whenRight res $ \_ ->
    L.logInfo "OnCancel" $ "OnCancel callback successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending onCancel Callback: " (show err)
  return $ first show res


onStatus :: OnStatusReq -> L.Flow (Either Text ())
onStatus req = do
  url <- getBaseUrl
  res <- L.callAPI url $ API.onStatus req
  whenRight res $ \_ ->
    L.logInfo "OnStatus" $ "OnStatus callback successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending onStatus Callback: " (show err)
  return $ first show res

getBaseUrl :: L.Flow BaseUrl
getBaseUrl = do
  url <- L.runIO $ getEnv "BECKN_GATEWAY_BASE_URL"
  port <- L.runIO $ getEnv "BECKN_GATEWAY_PORT"
  return $ defaultBaseUrl url $ read $ port

defaultBaseUrl :: String -> Int -> BaseUrl
defaultBaseUrl baseUrl port = do
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = baseUrl,
      baseUrlPort = port,
      baseUrlPath = "/v1"
    }
