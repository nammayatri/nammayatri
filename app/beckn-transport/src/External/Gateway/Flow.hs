module External.Gateway.Flow where

import App.Types
import Beckn.Types.API.Cancel
import Beckn.Types.API.Confirm
import Beckn.Types.API.Search
import Beckn.Types.API.Status
import Beckn.Types.API.Track
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.API as API
import Servant.Client
import System.Environment

onSearch :: OnSearchReq -> Flow (Either Text ())
onSearch req = do
  url <- getBaseUrl
  res <- L.callAPI url $ API.onSearch req
  whenRight res $ \_ ->
    L.logInfo "OnSearch" "OnSearch callback successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending onSearch Callback: " (show err)
  return $ first show res

onTrackTrip :: OnTrackTripReq -> Flow (Either Text ())
onTrackTrip req = do
  url <- getBaseUrl
  res <- L.callAPI url $ API.onTrackTrip req
  whenRight res $ \_ ->
    L.logInfo "OnTrackTrip" "OnTrackTrip callback successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending OnTrackTrip Callback: " (show err)
  return $ first show res

onConfirm :: OnConfirmReq -> Flow (Either Text ())
onConfirm req = do
  url <- getBaseUrl
  res <- L.callAPI url $ API.onConfirm req
  whenRight res $ \_ ->
    L.logInfo "OnConfirm" "OnConfirm callback successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending onConfirm Callback: " (show err)
  return $ first show res

onCancel :: OnCancelReq -> Flow (Either Text ())
onCancel req = do
  url <- getBaseUrl
  res <- L.callAPI url $ API.onCancel req
  whenRight res $ \_ ->
    L.logInfo "OnCancel" "OnCancel callback successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending onCancel Callback: " (show err)
  return $ first show res

onStatus :: OnStatusReq -> Flow (Either Text ())
onStatus req = do
  url <- getBaseUrl
  res <- L.callAPI url $ API.onStatus req
  whenRight res $ \_ ->
    L.logInfo "OnStatus" "OnStatus callback successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending onStatus Callback: " (show err)
  return $ first show res

getBaseUrl :: Flow BaseUrl
getBaseUrl = L.runIO $ do
  host <- fromMaybe "localhost" <$> lookupEnv "GATEWAY_HOST"
  port <- fromMaybe 8013 . (>>= readMaybe) <$> lookupEnv "GATEWAY_PORT"
  path <- fromMaybe "/v1" <$> lookupEnv "GATEWAY_PATH"
  return $
    BaseUrl Http host port path
