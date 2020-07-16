module External.Gateway.Flow where

import App.Types
import qualified Beckn.Types.API.Cancel as Cancel
import qualified Beckn.Types.API.Confirm as Confirm
import Beckn.Types.API.Search
import qualified Beckn.Types.API.Track as Track
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Types as API
import Servant.Client
import System.Environment
import Types.API.Location

search ::
  BaseUrl -> SearchReq -> Flow (Either Text ())
search url req = do
  res <- L.callAPI url $ API.search req
  whenRight res $ \_ ->
    L.logInfo "Search" "Search successfully delivered"
  whenLeft res $ \err ->
    L.logError "Search" ("error occurred while search: " <> show err)
  return $ first show res

confirm :: BaseUrl -> Confirm.ConfirmReq -> Flow (Either Text ())
confirm url req = do
  res <- L.callAPI url $ API.confirm req
  whenLeft res $ \err ->
    L.logError "error occurred while confirm: " (show err)
  whenLeft res $ \err ->
    L.logError "Confirm" ("error occurred while confirm: " <> show err)
  return $ first show res

location :: BaseUrl -> Text -> Flow (Either Text GetLocationRes)
location url req = do
  res <- L.callAPI url $ API.location req
  whenLeft res $ \err ->
    L.logError "error occurred while confirm: " (show err)
  whenLeft res $ \err ->
    L.logError "Location" ("error occurred while getting location: " <> show err)
  return $ first show res

track :: BaseUrl -> Track.TrackTripReq -> Flow (Either Text ())
track url req = do
  res <- L.callAPI url $ API.trackTrip req
  case res of
    Left err -> L.logError "error occurred while track trip: " (show err)
    Right _ -> L.logInfo "Track" "Track successfully delivered"
  return $ first show res

cancel :: BaseUrl -> Cancel.CancelReq -> Flow (Either Text ())
cancel url req = do
  res <- L.callAPI url $ API.cancel req
  case res of
    Left err -> L.logError "error occurred while cancel trip: " (show err)
    Right _ -> L.logInfo "Cancel" "Cancel successfully delivered"
  return $ first show res

getBaseUrl :: Flow BaseUrl
getBaseUrl = L.runIO $ do
  host <- fromMaybe "localhost" <$> lookupEnv "GATEWAY_HOST"
  port <- fromMaybe 8014 . (>>= readMaybe) <$> lookupEnv "GATEWAY_PORT"
  path <- fromMaybe "/v1" <$> lookupEnv "GATEWAY_PATH"
  return $
    BaseUrl Http host port path
