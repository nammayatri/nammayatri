module ExternalAPI.Flow where

import App.Types
import Beckn.Types.Core.API.Call
import Beckn.Types.Core.API.Cancel
import Beckn.Types.Core.API.Confirm
import Beckn.Types.Core.API.Search
import Beckn.Types.Core.API.Status
import Beckn.Types.Core.API.Track
import Beckn.Types.Core.API.Update
import Beckn.Types.Id
import Beckn.Utils.Error.BecknAPIError
import EulerHS.Prelude
import qualified ExternalAPI.Types as API
import Storage.Queries.Organization as Org
import Types.Error
import Utils.Auth
import Utils.Common

onSearch :: OnSearchReq -> Text -> Flow ()
onSearch req bppShortId = do
  appConfig <- ask
  authKey <- getHttpManagerKey bppShortId
  gatewayShortId <- xGatewaySelector appConfig & fromMaybeM GatewaySelectorNotSet
  gatewayOrg <- Org.findOrgByShortId $ ShortId gatewayShortId
  callbackUrl <- case gatewayShortId of
    "NSDL.BG.1" -> appConfig.xGatewayNsdlUrl & fromMaybeM NSDLBaseUrlNotSet
    "JUSPAY.BG.1" -> gatewayOrg.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
    _ -> throwError UnsupportedGatewaySelector
  callBecknAPI (Just authKey) Nothing callbackUrl (API.onSearch req) "on_search"

onTrackTrip :: BaseUrl -> OnTrackTripReq -> Text -> Flow ()
onTrackTrip url req bppShortId = do
  authKey <- getHttpManagerKey bppShortId
  callBecknAPI (Just authKey) Nothing url (API.onTrackTrip req) "on_track"

onUpdate :: BaseUrl -> OnUpdateReq -> Text -> Flow ()
onUpdate url req bppShortId = do
  authKey <- getHttpManagerKey bppShortId
  callBecknAPI (Just authKey) Nothing url (API.onUpdate req) "on_update"

onConfirm :: BaseUrl -> OnConfirmReq -> Text -> Flow ()
onConfirm url req bppShortId = do
  authKey <- getHttpManagerKey bppShortId
  callBecknAPI (Just authKey) Nothing url (API.onConfirm req) "on_confirm"

onCancel :: BaseUrl -> OnCancelReq -> Text -> Flow ()
onCancel url req bppShortId = do
  authKey <- getHttpManagerKey bppShortId
  callBecknAPI (Just authKey) Nothing url (API.onCancel req) "on_cancel"

onStatus :: BaseUrl -> OnStatusReq -> Text -> Flow ()
onStatus url req bppShortId = do
  authKey <- getHttpManagerKey bppShortId
  callBecknAPI (Just authKey) Nothing url (API.onStatus req) "on_status"

initiateCall :: CallReq -> Flow ()
initiateCall req = do
  url <- xAppUri <$> ask
  callBecknAPI Nothing (Just "UNABLE_TO_CALL") url (API.initiateCall req) "call/to_customer"
