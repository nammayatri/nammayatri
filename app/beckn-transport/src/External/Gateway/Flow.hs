module External.Gateway.Flow where

import App.Types
import Beckn.Types.API.Call
import Beckn.Types.API.Cancel
import Beckn.Types.API.Confirm
import Beckn.Types.API.Search
import Beckn.Types.API.Status
import Beckn.Types.API.Track
import Beckn.Types.API.Update
import qualified Data.Text as T (pack)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.API as API
import Servant.Client
import System.Environment
import Utils.Common

onSearch :: OnSearchReq -> Flow (Either Text ())
onSearch req = do
  url <- getGatewayBaseUrl
  apiKey <- L.runIO $ lookupEnv "BG_API_KEY"
  res <- callAPI url (API.onSearch (maybe "mobility-provider-key" T.pack apiKey) req) "on_search"
  whenRight res $ \_ ->
    L.logInfo "OnSearch" "OnSearch callback successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending onSearch Callback: " (show err)
  return $ first show res

onTrackTrip :: OnTrackTripReq -> Flow (Either Text ())
onTrackTrip req = do
  url <- getAppBaseUrl
  res <- callAPI url (API.onTrackTrip req) "on_track"
  whenRight res $ \_ ->
    L.logInfo "OnTrackTrip" "OnTrackTrip callback successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending OnTrackTrip Callback: " (show err)
  return $ first show res

onUpdate :: OnUpdateReq -> Flow (Either Text ())
onUpdate req = do
  url <- getAppBaseUrl
  res <- callAPI url (API.onUpdate req) "on_update"
  whenRight res $ \_ ->
    L.logInfo "OnUpdate" "OnUpdate callback successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending OnUpdate Callback: " (show err)
  return $ first show res

onConfirm :: OnConfirmReq -> Flow (Either Text ())
onConfirm req = do
  url <- getAppBaseUrl
  res <- callAPI url (API.onConfirm req) "on_confirm"
  whenRight res $ \_ ->
    L.logInfo "OnConfirm" "OnConfirm callback successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending onConfirm Callback: " (show err)
  return $ first show res

onCancel :: OnCancelReq -> Flow (Either Text ())
onCancel req = do
  url <- getAppBaseUrl
  res <- callAPI url (API.onCancel req) "on_cancel"
  whenRight res $ \_ ->
    L.logInfo "OnCancel" "OnCancel callback successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending onCancel Callback: " (show err)
  return $ first show res

onStatus :: OnStatusReq -> Flow (Either Text ())
onStatus req = do
  url <- getAppBaseUrl
  res <- callAPI url (API.onStatus req) "on_status"
  whenRight res $ \_ ->
    L.logInfo "OnStatus" "OnStatus callback successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending onStatus Callback: " (show err)
  return $ first show res

getGatewayBaseUrl :: Flow BaseUrl
getGatewayBaseUrl = L.runIO $ do
  host <- fromMaybe "localhost" <$> lookupEnv "BECKN_GATEWAY_HOST"
  port <- fromMaybe 8015 . (>>= readMaybe) <$> lookupEnv "BECKN_GATEWAY_PORT"
  path <- fromMaybe "/v1" <$> lookupEnv "BECKN_GATEWAY_PATH"
  return $
    BaseUrl Http host port path

getAppBaseUrl :: Flow BaseUrl
getAppBaseUrl = L.runIO $ do
  host <- fromMaybe "localhost" <$> lookupEnv "BECKN_APP_HOST"
  port <- fromMaybe 8013 . (>>= readMaybe) <$> lookupEnv "BECKN_APP_PORT"
  path <- fromMaybe "/v1" <$> lookupEnv "BECKN_APP_PATH"
  return $
    BaseUrl Http host port path

initiateCall :: CallReq -> Flow (Either Text ())
initiateCall req = do
  url <- getAppBaseUrl
  res <- L.callAPI url $ API.initiateCall req
  whenRight res $ \_ ->
    L.logInfo "initiateCall" "initiateCall successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending initiateCall: " (show err)
  return $ first show res
