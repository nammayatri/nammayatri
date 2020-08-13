{-# LANGUAGE TypeApplications #-}

module External.Gateway.Flow where

import App.Types
import Beckn.Types.API.Cancel
import Beckn.Types.API.Confirm
import Beckn.Types.API.Search
import Beckn.Types.API.Status
import Beckn.Types.API.Track
import Beckn.Types.Common
import Beckn.Types.Core.Error
import Beckn.Utils.Servant.Trail.Client (callAPIWithTrail)
import qualified Data.Text as T (pack)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Types as API
import Servant.Client
import System.Environment
import Types.API.Location

search ::
  BaseUrl -> SearchReq -> Flow (Either Text ())
search url req = do
  mNsdlUrl <- L.runIO $ lookupEnv "NSDL_GATEWAY_URL"
  let mNsdlBaseUrl = mNsdlUrl >>= parseBaseUrl
  res <- case mNsdlBaseUrl of
    Just nsdlBaseUrl -> do
      nsdlBapId <- L.runIO $ lookupEnv "NSDL_BAP_ID"
      nsdlBapPwd <- L.runIO $ lookupEnv "NSDL_BAP_PASSWORD"
      callAPIWithTrail nsdlBaseUrl (API.nsdlSearch (T.pack <$> nsdlBapId) (T.pack <$> nsdlBapPwd) req) "search"
    Nothing -> do
      apiKey <- L.runIO $ lookupEnv "BG_API_KEY"
      callAPIWithTrail url (API.search (maybe "mobility-app-key" T.pack apiKey) req) "search"
  case res of
    Left err -> do
      L.logError @Text "Search" ("error occurred while search: " <> show err)
      return $ Left $ show err
    Right _ -> do
      L.logInfo @Text "Search" "Search successfully delivered"
      return $ Right ()

confirm :: BaseUrl -> ConfirmReq -> Flow AckResponse
confirm url req@ConfirmReq {context} = do
  res <- callAPIWithTrail url (API.confirm req) "confirm"
  whenLeft res $ \err ->
    L.logError @Text "error occurred while confirm: " (show err)
  whenLeft res $ \err ->
    L.logError @Text "Confirm" ("error occurred while confirm: " <> show err)
  case res of
    Left err -> return $ AckResponse context (ack "ACK") $ Just (domainError (show err))
    Right _ -> return $ AckResponse context (ack "ACK") Nothing

location :: BaseUrl -> Text -> Flow (Either Text GetLocationRes)
location url req = do
  res <- callAPIWithTrail url (API.location req) "location"
  whenLeft res $ \err ->
    L.logError @Text "error occurred while confirm: " (show err)
  whenLeft res $ \err ->
    L.logError @Text "Location" ("error occurred while getting location: " <> show err)
  return $ first show res

track :: BaseUrl -> TrackTripReq -> Flow AckResponse
track url req@TrackTripReq {context} = do
  res <- callAPIWithTrail url (API.trackTrip req) "track"
  case res of
    Left err -> L.logError @Text "error occurred while track trip: " (show err)
    Right _ -> L.logInfo @Text "Track" "Track successfully delivered"
  case res of
    Left err -> return $ AckResponse context (ack "ACK") $ Just (domainError (show err))
    Right _ -> return $ AckResponse context (ack "ACK") Nothing

cancel :: BaseUrl -> CancelReq -> Flow (Either Text ())
cancel url req = do
  res <- callAPIWithTrail url (API.cancel req) "cancel"
  case res of
    Left err -> do
      L.logError @Text "error occurred while cancel trip: " (show err)
      return $ Left $ show err
    Right _ -> do
      L.logInfo @Text "Cancel" "Cancel successfully delivered"
      return $ Right ()

status :: BaseUrl -> StatusReq -> Flow AckResponse
status url req@StatusReq {context} = do
  res <- callAPIWithTrail url (API.status req) "status"
  case res of
    Left err -> L.logError @Text "error occurred while getting status: " (show err)
    Right _ -> L.logInfo @Text "Status" "Status successfully delivered"
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

getProviderBaseUrl :: Flow BaseUrl
getProviderBaseUrl = L.runIO $ do
  host <- fromMaybe "localhost" <$> lookupEnv "BECKN_PROVIDER_HOST"
  port <- fromMaybe 8014 . (>>= readMaybe) <$> lookupEnv "BECKN_PROVIDER_PORT"
  path <- fromMaybe "/v1" <$> lookupEnv "BECKN_PROVIDER_PATH"
  return $
    BaseUrl Http host port path
