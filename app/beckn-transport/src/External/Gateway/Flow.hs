{-# LANGUAGE OverloadedLabels #-}

module External.Gateway.Flow where

import App.Types
import Beckn.Types.App (ShortOrganizationId (..))
import Beckn.Types.Core.API.Call
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.API.Cancel
import Beckn.Types.Core.API.Confirm
import Beckn.Types.Core.API.Search
import Beckn.Types.Core.API.Status
import Beckn.Types.Core.API.Track
import Beckn.Types.Core.API.Update
import Beckn.Types.Core.Ack
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (signatureAuthManagerKey)
import Beckn.Utils.Servant.Trail.Client (callAPIWithTrail, callAPIWithTrail')
import EulerHS.Prelude
import qualified External.Gateway.API as API
import Servant.Client (BaseUrl)
import Storage.Queries.Organization as Org

onSearch :: OnSearchReq -> Flow AckResponse
onSearch req@CallbackReq {context} = do
  appConfig <- ask
  gatewayShortId <- xGatewaySelector appConfig & fromMaybeM500 "GATEWAY_SELECTOR_NOT_SET"
  gatewayOrg <- Org.findOrgByShortId $ ShortOrganizationId gatewayShortId
  res <- case gatewayShortId of
    "NSDL.BG.1" -> do
      nsdlBaseUrl <- xGatewayNsdlUrl appConfig & fromMaybeM500 "NSDL_BASEURL_NOT_SET"
      callAPIWithTrail' (Just signatureAuthManagerKey) nsdlBaseUrl (API.nsdlOnSearch req) "on_search"
    "JUSPAY.BG.1" -> do
      callbackUrl <- gatewayOrg ^. #_callbackUrl & fromMaybeM500 "CALLBACK_URL_NOT_CONFIGURED"
      callAPIWithTrail' (Just signatureAuthManagerKey) callbackUrl (API.onSearch req) "on_search"
    _ -> throwError500 "gateway not configured"
  AckResponse {} <- checkClientError context res
  mkOkResponse context

onTrackTrip :: BaseUrl -> OnTrackTripReq -> Flow AckResponse
onTrackTrip url req@CallbackReq {context} = do
  res <- callAPIWithTrail' (Just signatureAuthManagerKey) url (API.onTrackTrip req) "on_track"
  -- TODO: can we just return AckResponse returned by client call?
  -- Will it have the same context?
  AckResponse {} <- checkClientError context res
  mkOkResponse context

onUpdate :: BaseUrl -> OnUpdateReq -> Flow AckResponse
onUpdate url req@CallbackReq {context} = do
  res <- callAPIWithTrail' (Just signatureAuthManagerKey) url (API.onUpdate req) "on_update"
  AckResponse {} <- checkClientError context res
  mkOkResponse context

onConfirm :: BaseUrl -> OnConfirmReq -> Flow AckResponse
onConfirm url req@CallbackReq {context} = do
  res <- callAPIWithTrail' (Just signatureAuthManagerKey) url (API.onConfirm req) "on_confirm"
  AckResponse {} <- checkClientError context res
  mkOkResponse context

onCancel :: BaseUrl -> OnCancelReq -> Flow AckResponse
onCancel url req@CallbackReq {context} = do
  res <- callAPIWithTrail' (Just signatureAuthManagerKey) url (API.onCancel req) "on_cancel"
  AckResponse {} <- checkClientError context res
  mkOkResponse context

onStatus :: BaseUrl -> OnStatusReq -> Flow AckResponse
onStatus url req@CallbackReq {context} = do
  res <- callAPIWithTrail' (Just signatureAuthManagerKey) url (API.onStatus req) "on_status"
  AckResponse {} <- checkClientError context res
  mkOkResponse context

initiateCall :: CallReq -> Flow AckResponse
initiateCall req@CallReq {context} = do
  url <- xAppUri <$> ask
  res <- callAPIWithTrail url (API.initiateCall req) "call_to_customer"
  AckResponse {} <- checkClientError context res
  mkOkResponse context
