{-# LANGUAGE OverloadedLabels #-}

module External.Gateway.Flow where

import App.Types
import Beckn.Types.App (ShortOrganizationId (..))
import Beckn.Types.Common
import Beckn.Types.Core.API.Call
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.API.Cancel
import Beckn.Types.Core.API.Confirm
import Beckn.Types.Core.API.Search
import Beckn.Types.Core.API.Status
import Beckn.Types.Core.API.Track
import Beckn.Types.Core.API.Update
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
      callbackApiKey <- gatewayOrg ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
      callbackUrl <- gatewayOrg ^. #_callbackUrl & fromMaybeM500 "CALLBACK_URL_NOT_CONFIGURED"
      callAPIWithTrail' (Just signatureAuthManagerKey) callbackUrl (API.onSearch callbackApiKey req) "on_search"
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
