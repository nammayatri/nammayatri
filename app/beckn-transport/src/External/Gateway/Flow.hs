{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

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
import Beckn.Utils.Servant.Trail.Client (callAPIWithTrail, callAPIWithTrail')
import EulerHS.Prelude
import qualified External.Gateway.API as API
import Servant.Client (BaseUrl)
import Storage.Queries.Organization as Org
import Utils.Auth

onSearch :: OnSearchReq -> Text -> Flow AckResponse
onSearch req@CallbackReq {context} bppShortId = do
  appConfig <- ask
  authKey <- getHttpManagerKey bppShortId
  gatewayShortId <- xGatewaySelector appConfig & fromMaybeM500 "GATEWAY_SELECTOR_NOT_SET"
  gatewayOrg <- Org.findOrgByShortId $ ShortOrganizationId gatewayShortId
  res <- case gatewayShortId of
    "NSDL.BG.1" -> do
      nsdlBaseUrl <- xGatewayNsdlUrl appConfig & fromMaybeM500 "NSDL_BASEURL_NOT_SET"
      callAPIWithTrail' (Just authKey) nsdlBaseUrl (API.nsdlOnSearch req) "on_search"
    "JUSPAY.BG.1" -> do
      callbackUrl <- gatewayOrg ^. #_callbackUrl & fromMaybeM500 "CALLBACK_URL_NOT_CONFIGURED"
      callAPIWithTrail' (Just authKey) callbackUrl (API.onSearch req) "on_search"
    _ -> throwError500 "gateway not configured"
  AckResponse {} <- checkClientError context res
  mkOkResponse context

onTrackTrip :: BaseUrl -> OnTrackTripReq -> Text -> Flow AckResponse
onTrackTrip url req@CallbackReq {context} bppShortId = do
  authKey <- getHttpManagerKey bppShortId
  res <- callAPIWithTrail' (Just authKey) url (API.onTrackTrip req) "on_track"
  -- TODO: can we just return AckResponse returned by client call?
  -- Will it have the same context?
  AckResponse {} <- checkClientError context res
  mkOkResponse context

onUpdate :: BaseUrl -> OnUpdateReq -> Text -> Flow AckResponse
onUpdate url req@CallbackReq {context} bppShortId = do
  authKey <- getHttpManagerKey bppShortId
  res <- callAPIWithTrail' (Just authKey) url (API.onUpdate req) "on_update"
  AckResponse {} <- checkClientError context res
  mkOkResponse context

onConfirm :: BaseUrl -> OnConfirmReq -> Text -> Flow AckResponse
onConfirm url req@CallbackReq {context} bppShortId = do
  authKey <- getHttpManagerKey bppShortId
  res <- callAPIWithTrail' (Just authKey) url (API.onConfirm req) "on_confirm"
  AckResponse {} <- checkClientError context res
  mkOkResponse context

onCancel :: BaseUrl -> OnCancelReq -> Text -> Flow AckResponse
onCancel url req@CallbackReq {context} bppShortId = do
  authKey <- getHttpManagerKey bppShortId
  res <- callAPIWithTrail' (Just authKey) url (API.onCancel req) "on_cancel"
  AckResponse {} <- checkClientError context res
  mkOkResponse context

onStatus :: BaseUrl -> OnStatusReq -> Text -> Flow AckResponse
onStatus url req@CallbackReq {context} bppShortId = do
  authKey <- getHttpManagerKey bppShortId
  res <- callAPIWithTrail' (Just authKey) url (API.onStatus req) "on_status"
  AckResponse {} <- checkClientError context res
  mkOkResponse context

initiateCall :: CallReq -> Flow Ack
initiateCall req = do
  url <- xAppUri <$> ask
  res <- callAPIWithTrail url (API.initiateCall req) "call_to_customer"
  case res of
    Right x -> return x
    Left cliErr -> do
      let err = fromClientError cliErr
      logError "client call error" $ (err ^. #_message) ?: "Some error"
      throwError500 "Call API error"
