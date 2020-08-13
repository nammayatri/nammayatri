{-# LANGUAGE TypeApplications #-}

module External.Gateway.Flow where

import App.Types
import Beckn.Types.API.Call
import Beckn.Types.API.Callback
import Beckn.Types.API.Cancel
import Beckn.Types.API.Confirm
import Beckn.Types.API.Search
import Beckn.Types.API.Status
import Beckn.Types.API.Track
import Beckn.Types.API.Update
import Beckn.Types.Common
import Beckn.Types.Core.Error
import Beckn.Utils.Servant.Trail.Client (callAPIWithTrail)
import qualified Data.Text as T (pack)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.API as API
import Servant.Client
import System.Environment

onSearch :: OnSearchReq -> Flow AckResponse
onSearch req@CallbackReq {context} = do
  mNsdlUrl <- L.runIO $ lookupEnv "NSDL_GATEWAY_URL"
  let mNsdlBaseUrl = mNsdlUrl >>= parseBaseUrl
  res <- case mNsdlBaseUrl of
    Just nsdlBaseUrl -> do
      nsdlBppId <- L.runIO $ lookupEnv "NSDL_BPP_ID"
      nsdlBppPwd <- L.runIO $ lookupEnv "NSDL_BPP_PASSWORD"
      callAPIWithTrail nsdlBaseUrl (API.nsdlOnSearch (T.pack <$> nsdlBppId) (T.pack <$> nsdlBppPwd) req) "on_search"
    Nothing -> do
      url <- getGatewayBaseUrl
      apiKey <- L.runIO $ lookupEnv "BG_API_KEY"
      callAPIWithTrail url (API.onSearch (maybe "mobility-provider-key" T.pack apiKey) req) "on_search"
  whenRight res $ \_ ->
    L.logInfo @Text "OnSearch" "OnSearch callback successfully delivered"
  whenLeft res $ \err ->
    L.logError @Text "error occurred while sending onSearch Callback: " (show err)
  case res of
    Left err -> return $ AckResponse context (ack "ACK") $ Just (domainError (show err))
    Right _ -> return $ AckResponse context (ack "ACK") Nothing

onTrackTrip :: OnTrackTripReq -> Flow AckResponse
onTrackTrip req@CallbackReq {context} = do
  url <- getAppBaseUrl
  res <- callAPIWithTrail url (API.onTrackTrip req) "on_track"
  whenRight res $ \_ ->
    L.logInfo @Text "OnTrackTrip" "OnTrackTrip callback successfully delivered"
  whenLeft res $ \err ->
    L.logError @Text "error occurred while sending OnTrackTrip Callback: " (show err)
  case res of
    Left err -> return $ AckResponse context (ack "ACK") $ Just (domainError (show err))
    Right _ -> return $ AckResponse context (ack "ACK") Nothing

onUpdate :: OnUpdateReq -> Flow AckResponse
onUpdate req@CallbackReq {context} = do
  url <- getAppBaseUrl
  res <- callAPIWithTrail url (API.onUpdate req) "on_update"
  whenRight res $ \_ ->
    L.logInfo @Text "OnUpdate" "OnUpdate callback successfully delivered"
  whenLeft res $ \err ->
    L.logError @Text "error occurred while sending OnUpdate Callback: " (show err)
  case res of
    Left err -> return $ AckResponse context (ack "ACK") $ Just (domainError (show err))
    Right _ -> return $ AckResponse context (ack "ACK") Nothing

onConfirm :: OnConfirmReq -> Flow AckResponse
onConfirm req@CallbackReq {context} = do
  url <- getAppBaseUrl
  res <- callAPIWithTrail url (API.onConfirm req) "on_confirm"
  whenRight res $ \_ ->
    L.logInfo @Text "OnConfirm" "OnConfirm callback successfully delivered"
  whenLeft res $ \err ->
    L.logError @Text "error occurred while sending onConfirm Callback: " (show err)
  case res of
    Left err -> return $ AckResponse context (ack "ACK") $ Just (domainError (show err))
    Right _ -> return $ AckResponse context (ack "ACK") Nothing

onCancel :: OnCancelReq -> Flow AckResponse
onCancel req@CallbackReq {context} = do
  url <- getAppBaseUrl
  res <- callAPIWithTrail url (API.onCancel req) "on_cancel"
  whenRight res $ \_ ->
    L.logInfo @Text "OnCancel" "OnCancel callback successfully delivered"
  whenLeft res $ \err ->
    L.logError @Text "error occurred while sending onCancel Callback: " (show err)
  case res of
    Left err -> return $ AckResponse context (ack "ACK") $ Just (domainError (show err))
    Right _ -> return $ AckResponse context (ack "ACK") Nothing

onStatus :: OnStatusReq -> Flow AckResponse
onStatus req@CallbackReq {context} = do
  url <- getAppBaseUrl
  res <- callAPIWithTrail url (API.onStatus req) "on_status"
  whenRight res $ \_ ->
    L.logInfo @Text "OnStatus" "OnStatus callback successfully delivered"
  whenLeft res $ \err ->
    L.logError @Text "error occurred while sending onStatus Callback: " (show err)
  case res of
    Left err -> return $ AckResponse context (ack "ACK") $ Just (domainError (show err))
    Right _ -> return $ AckResponse context (ack "ACK") Nothing

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

initiateCall :: CallReq -> Flow AckResponse
initiateCall req@CallReq {context} = do
  url <- getAppBaseUrl
  res <- callAPIWithTrail url (API.initiateCall req) "call_to_customer"
  whenRight res $ \_ ->
    L.logInfo @Text "initiateCall" "initiateCall successfully delivered"
  whenLeft res $ \err ->
    L.logError @Text "error occurred while sending initiateCall: " (show err)
  case res of
    Left err -> return $ AckResponse context (ack "ACK") $ Just (domainError (show err))
    Right _ -> return $ AckResponse context (ack "ACK") Nothing
