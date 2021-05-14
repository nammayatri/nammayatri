{-# LANGUAGE OverloadedLabels #-}

module External.Gateway.Flow where

import App.Types
import Beckn.Types.Core.API.Call
import Beckn.Types.Core.API.Cancel
import Beckn.Types.Core.API.Confirm
import Beckn.Types.Core.API.Search
import Beckn.Types.Core.API.Status
import Beckn.Types.Core.API.Track
import Beckn.Types.Core.API.Update
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import Beckn.Utils.Servant.Trail.Client (callAPIWithTrail, callAPIWithTrail')
import EulerHS.Prelude
import qualified External.Gateway.API as API
import Servant.Client (BaseUrl)
import Storage.Queries.Organization as Org
import Types.Error
import Utils.Auth
import Utils.Common

onSearch :: OnSearchReq -> Text -> Flow AckResponse
onSearch req bppShortId = do
  appConfig <- ask
  authKey <- getHttpManagerKey bppShortId
  gatewayShortId <- xGatewaySelector appConfig & fromMaybeM GatewaySelectorNotSet
  gatewayOrg <- Org.findOrgByShortId $ ShortId gatewayShortId
  case gatewayShortId of
    "NSDL.BG.1" -> do
      nsdlBaseUrl <- xGatewayNsdlUrl appConfig & fromMaybeM NSDLBaseUrlNotSet
      callAPIWithTrail' (Just authKey) nsdlBaseUrl (API.nsdlOnSearch req) "on_search"
        >>= fromEitherM (ExternalAPICallError nsdlBaseUrl)
    "JUSPAY.BG.1" -> do
      callbackUrl <- gatewayOrg ^. #_callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
      callAPIWithTrail' (Just authKey) callbackUrl (API.onSearch req) "on_search"
        >>= fromEitherM (ExternalAPICallError callbackUrl)
    _ -> throwError GatewaySelectorNotSet

onTrackTrip :: BaseUrl -> OnTrackTripReq -> Text -> Flow AckResponse
onTrackTrip url req bppShortId = do
  authKey <- getHttpManagerKey bppShortId
  callAPIWithTrail' (Just authKey) url (API.onTrackTrip req) "on_track"
    >>= fromEitherM (ExternalAPICallError url)

-- TODO: can we just return AckResponse returned by client call?
-- Will it have the same context?

onUpdate :: BaseUrl -> OnUpdateReq -> Text -> Flow AckResponse
onUpdate url req bppShortId = do
  authKey <- getHttpManagerKey bppShortId
  callAPIWithTrail' (Just authKey) url (API.onUpdate req) "on_update"
    >>= fromEitherM (ExternalAPICallError url)

onConfirm :: BaseUrl -> OnConfirmReq -> Text -> Flow AckResponse
onConfirm url req bppShortId = do
  authKey <- getHttpManagerKey bppShortId
  callAPIWithTrail' (Just authKey) url (API.onConfirm req) "on_confirm"
    >>= fromEitherM (ExternalAPICallError url)

onCancel :: BaseUrl -> OnCancelReq -> Text -> Flow AckResponse
onCancel url req bppShortId = do
  authKey <- getHttpManagerKey bppShortId
  callAPIWithTrail' (Just authKey) url (API.onCancel req) "on_cancel"
    >>= fromEitherM (ExternalAPICallError url)

onStatus :: BaseUrl -> OnStatusReq -> Text -> Flow AckResponse
onStatus url req bppShortId = do
  authKey <- getHttpManagerKey bppShortId
  callAPIWithTrail' (Just authKey) url (API.onStatus req) "on_status"
    >>= fromEitherM (ExternalAPICallError url)

initiateCall :: CallReq -> Flow Ack
initiateCall req = do
  url <- xAppUri <$> ask
  callAPIWithTrail url (API.initiateCall req) "call_to_customer"
    >>= fromEitherM (ExternalAPICallErrorWithCode "UNABLE_TO_CALL" url)
