module External.Gateway.Flow where

import App.Types
import Beckn.Types.Core.API.Cancel
import Beckn.Types.Core.API.Confirm
import Beckn.Types.Core.API.Feedback
import Beckn.Types.Core.API.Search
import Beckn.Types.Core.API.Status
import Beckn.Types.Core.API.Track
import Beckn.Types.Core.Ack (AckResponse (..))
import Beckn.Types.Error
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (signatureAuthManagerKey)
import Beckn.Utils.Servant.Trail.Client (callAPIWithTrail')
import EulerHS.Prelude
import qualified External.Gateway.Types as API
import Servant.Client
import Types.API.Location

search ::
  BaseUrl -> SearchReq -> Flow (Either Text AckResponse)
search url req = do
  mGatewaySelector <- xGatewaySelector <$> ask
  res <- case mGatewaySelector of
    Just "NSDL.BG.1" -> do
      mNsdlUrl <- xGatewayNsdlUrl <$> ask
      case mNsdlUrl of
        Just nsdlBaseUrl ->
          callAPIWithTrail' (Just signatureAuthManagerKey) nsdlBaseUrl (API.nsdlSearch req) "search"
        Nothing -> throwError NSDLBaseUrlNotSet
    Just "JUSPAY.BG.1" ->
      callAPIWithTrail' (Just signatureAuthManagerKey) url (API.search req) "search"
    _ -> throwError GatewaySelectorNotSet
  case res of
    Left err -> do
      logTagInfo "Search" ("error occurred while search: " <> show err)
    Right _ -> do
      logTagInfo "Search" "Search successfully delivered"
  return $ first show res

confirm :: BaseUrl -> ConfirmReq -> Flow (Either Text AckResponse)
confirm url req = do
  res <- callAPIWithTrail' (Just signatureAuthManagerKey) url (API.confirm req) "confirm"
  whenLeft res $ \err ->
    logTagError "Confirm" ("error occurred while confirm: " <> show err)
  return $ first show res

location :: BaseUrl -> Text -> Flow (Either Text GetLocationRes)
location url req = do
  -- TODO: fix authentication
  res <- callAPIWithTrail' Nothing url (API.location req) "location"
  whenLeft res $ \err ->
    logTagError "Location" ("error occurred while getting location: " <> show err)
  return $ first show res

track :: BaseUrl -> TrackTripReq -> Flow (Either Text AckResponse)
track url req = do
  res <- callAPIWithTrail' (Just signatureAuthManagerKey) url (API.trackTrip req) "track"
  case res of
    Left err -> logTagError "error occurred while track trip: " (show err)
    Right _ -> logTagInfo "Track" "Track successfully delivered"
  return $ first show res

cancel :: BaseUrl -> CancelReq -> Flow (Either Text AckResponse)
cancel url req = do
  res <- callAPIWithTrail' (Just signatureAuthManagerKey) url (API.cancel req) "cancel"
  case res of
    Left err -> do
      logTagError "error occurred while cancel trip: " (show err)
    Right _ -> do
      logTagInfo "Cancel" "Cancel successfully delivered"
  return $ first show res

status :: BaseUrl -> StatusReq -> Flow (Either Text AckResponse)
status url req = do
  res <- callAPIWithTrail' (Just signatureAuthManagerKey) url (API.status req) "status"
  case res of
    Left err -> logTagError "error occurred while getting status: " (show err)
    Right _ -> logTagInfo "Status" "Status successfully delivered"
  return $ first show res

feedback :: BaseUrl -> FeedbackReq -> Flow (Either Text AckResponse)
feedback url req = do
  res <- callAPIWithTrail' (Just signatureAuthManagerKey) url (API.feedback req) "feedback"
  case res of
    Left err -> do
      logTagError "Gateway" $ "Error occurred when sending feedback: " <> show err
    Right _ -> do
      logTagInfo "Gateway" "Feedback successfully sent."
  return $ first show res
