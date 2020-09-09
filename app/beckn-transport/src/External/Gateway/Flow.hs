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
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.API as API
import Servant (err500, errBody)

onSearch :: OnSearchReq -> Flow AckResponse
onSearch req@CallbackReq {context} = do
  env <- ask
  res <- case xGatewaySelector env of
    Just "NSDL" -> do
      let mNsdlBaseUrl = xGatewayNsdlUrl env
      case mNsdlBaseUrl of
        Just nsdlBaseUrl -> do
          callAPIWithTrail nsdlBaseUrl (API.nsdlOnSearch (nsdlUsername env) (nsdlPassword env) req) "on_search"
        Nothing -> L.throwException $ err500 {errBody = "invalid nsdl gateway url"}
    Just "JUSPAY" -> do
      let url = xGatewayUri env
      callAPIWithTrail url (API.onSearch (xGatewayApiKey env ?: "mobility-provider-key") req) "on_search"
    _ -> L.throwException $ err500 {errBody = "gateway not configured"}
  AckResponse {} <- checkClientError context res
  mkOkResponse context

onTrackTrip :: OnTrackTripReq -> Flow AckResponse
onTrackTrip req@CallbackReq {context} = do
  url <- xAppUri <$> ask
  res <- callAPIWithTrail url (API.onTrackTrip req) "on_track"
  -- TODO: can we just return AckResponse returned by client call?
  -- Will it have the same context?
  AckResponse {} <- checkClientError context res
  mkOkResponse context

onUpdate :: OnUpdateReq -> Flow AckResponse
onUpdate req@CallbackReq {context} = do
  url <- xAppUri <$> ask
  res <- callAPIWithTrail url (API.onUpdate req) "on_update"
  AckResponse {} <- checkClientError context res
  mkOkResponse context

onConfirm :: OnConfirmReq -> Flow AckResponse
onConfirm req@CallbackReq {context} = do
  url <- xAppUri <$> ask
  res <- callAPIWithTrail url (API.onConfirm req) "on_confirm"
  AckResponse {} <- checkClientError context res
  mkOkResponse context

onCancel :: OnCancelReq -> Flow AckResponse
onCancel req@CallbackReq {context} = do
  url <- xAppUri <$> ask
  res <- callAPIWithTrail url (API.onCancel req) "on_cancel"
  AckResponse {} <- checkClientError context res
  mkOkResponse context

onStatus :: OnStatusReq -> Flow AckResponse
onStatus req@CallbackReq {context} = do
  url <- xAppUri <$> ask
  res <- callAPIWithTrail url (API.onStatus req) "on_status"
  AckResponse {} <- checkClientError context res
  mkOkResponse context

initiateCall :: CallReq -> Flow AckResponse
initiateCall req@CallReq {context} = do
  url <- xAppUri <$> ask
  res <- callAPIWithTrail url (API.initiateCall req) "call_to_customer"
  AckResponse {} <- checkClientError context res
  mkOkResponse context
