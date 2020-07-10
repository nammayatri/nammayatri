module External.Gateway.Flow where

import Beckn.Types.API.Cancel
import Beckn.Types.API.Confirm
import Beckn.Types.API.Search
import Beckn.Types.API.Status
import Beckn.Types.API.Track
import Beckn.Types.Common
import qualified Data.Text as T
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.API as API
import External.Gateway.Types
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
getBaseUrl = do
  envUrl <- L.runIO loadGatewayUrl
  case envUrl of
    Nothing -> do
      L.logInfo "Gateway Url" "Using defaults"
      return $
        BaseUrl
          { baseUrlScheme = Http,
            baseUrlHost = "localhost",
            baseUrlPort = 8013,
            baseUrlPath = "/v1"
          }
    Just url -> return url

loadGatewayUrl :: IO (Maybe BaseUrl)
loadGatewayUrl = do
  mhost <- lookupEnv "BECKN_GATEWAY_BASE_URL"
  mport <- lookupEnv "BECKN_GATEWAY_PORT"
  pure $ do
    host <- mhost
    port <- mport
    p <- readMaybe port
    Just $
      BaseUrl
        { baseUrlScheme = Http, -- TODO: make Https when required
          baseUrlHost = host,
          baseUrlPort = p,
          baseUrlPath = "/v1"
        }
