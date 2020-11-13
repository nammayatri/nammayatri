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
import Beckn.Utils.Common
import Beckn.Utils.Servant.Trail.Client (callAPIWithTrail)
import EulerHS.Prelude
import qualified External.Gateway.API as API
import Servant.Client (BaseUrl)

onSearch :: BaseUrl -> Text -> OnSearchReq -> Flow AckResponse
onSearch url callbackApiKey req@CallbackReq {context} = do
  env <- ask
  res <- case xGatewaySelector env of
    Just "NSDL" ->
      callAPIWithTrail url (API.nsdlOnSearch (nsdlUsername env) (nsdlPassword env) req) "on_search"
    Just "JUSPAY" ->
      callAPIWithTrail url (API.onSearch callbackApiKey req) "on_search"
    _ -> throwError500 "gateway not configured"
  AckResponse {} <- checkClientError context res
  mkOkResponse context

onTrackTrip :: BaseUrl -> Text -> OnTrackTripReq -> Flow AckResponse
onTrackTrip url callbackApiKey req@CallbackReq {context} = do
  res <- callAPIWithTrail url (API.onTrackTrip callbackApiKey req) "on_track"
  -- TODO: can we just return AckResponse returned by client call?
  -- Will it have the same context?
  AckResponse {} <- checkClientError context res
  mkOkResponse context

onUpdate :: BaseUrl -> Text -> OnUpdateReq -> Flow AckResponse
onUpdate url callbackApiKey req@CallbackReq {context} = do
  res <- callAPIWithTrail url (API.onUpdate callbackApiKey req) "on_update"
  AckResponse {} <- checkClientError context res
  mkOkResponse context

onConfirm :: BaseUrl -> Text -> OnConfirmReq -> Flow AckResponse
onConfirm url callbackApiKey req@CallbackReq {context} = do
  res <- callAPIWithTrail url (API.onConfirm callbackApiKey req) "on_confirm"
  AckResponse {} <- checkClientError context res
  mkOkResponse context

onCancel :: BaseUrl -> Text -> OnCancelReq -> Flow AckResponse
onCancel url callbackApiKey req@CallbackReq {context} = do
  res <- callAPIWithTrail url (API.onCancel callbackApiKey req) "on_cancel"
  AckResponse {} <- checkClientError context res
  mkOkResponse context

onStatus :: BaseUrl -> Text -> OnStatusReq -> Flow AckResponse
onStatus url callbackApiKey req@CallbackReq {context} = do
  res <- callAPIWithTrail url (API.onStatus callbackApiKey req) "on_status"
  AckResponse {} <- checkClientError context res
  mkOkResponse context

initiateCall :: CallReq -> Flow AckResponse
initiateCall req@CallReq {context} = do
  url <- xAppUri <$> ask
  res <- callAPIWithTrail url (API.initiateCall req) "call_to_customer"
  AckResponse {} <- checkClientError context res
  mkOkResponse context
