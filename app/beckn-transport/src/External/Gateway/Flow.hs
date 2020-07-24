module External.Gateway.Flow where

import App.Types
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

onSearch :: OnSearchReq -> Flow (Either Text ())
onSearch req = do
  url <- getGatewayBaseUrl
  apiKey <- L.runIO $ lookupEnv "BP_API_KEY"
  res <- L.callAPI url $ API.onSearch (maybe "mobility-provider-key" T.pack apiKey) req
  whenRight res $ \_ ->
    L.logInfo "OnSearch" "OnSearch callback successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending onSearch Callback: " (show err)
  return $ first show res

onTrackTrip :: OnTrackTripReq -> Flow (Either Text ())
onTrackTrip req = do
  url <- getAppBaseUrl
  res <- L.callAPI url $ API.onTrackTrip req
  whenRight res $ \_ ->
    L.logInfo "OnTrackTrip" "OnTrackTrip callback successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending OnTrackTrip Callback: " (show err)
  return $ first show res

onUpdate :: OnUpdateReq -> Flow (Either Text ())
onUpdate req = do
  url <- getAppBaseUrl
  res <- L.callAPI url $ API.onUpdate req
  whenRight res $ \_ ->
    L.logInfo "OnUpdate" "OnUpdate callback successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending OnUpdate Callback: " (show err)
  return $ first show res

onConfirm :: OnConfirmReq -> Flow (Either Text ())
onConfirm req = do
  url <- getAppBaseUrl
  res <- L.callAPI url $ API.onConfirm req
  whenRight res $ \_ ->
    L.logInfo "OnConfirm" "OnConfirm callback successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending onConfirm Callback: " (show err)
  return $ first show res

onCancel :: OnCancelReq -> Flow (Either Text ())
onCancel req = do
  url <- getAppBaseUrl
  res <- L.callAPI url $ API.onCancel req
  whenRight res $ \_ ->
    L.logInfo "OnCancel" "OnCancel callback successfully delivered"
  whenLeft res $ \err ->
    L.logError "error occurred while sending onCancel Callback: " (show err)
  return $ first show res

onStatus :: OnStatusReq -> Flow (Either Text ())
onStatus req = do
  url <- getAppBaseUrl
  res <- L.callAPI url $ API.onStatus req
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
