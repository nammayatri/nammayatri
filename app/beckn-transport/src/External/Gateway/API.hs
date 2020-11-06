module External.Gateway.API where

import Beckn.Types.API.Call
import Beckn.Types.API.Cancel
import Beckn.Types.API.Confirm
import Beckn.Types.API.Search
import Beckn.Types.API.Status
import Beckn.Types.API.Track
import Beckn.Types.API.Update
import Beckn.Types.Common (AckResponse)
import Beckn.Utils.Servant.Trail.Client (RequestInfo, withClientTracing)
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant

onSearch :: Text -> OnSearchReq -> (RequestInfo, ET.EulerClient AckResponse)
onSearch = ET.client $ withClientTracing onSearchAPI

nsdlOnSearch :: Maybe Text -> Maybe Text -> OnSearchReq -> (RequestInfo, ET.EulerClient AckResponse)
nsdlOnSearch = ET.client $ withClientTracing nsdlOnSearchAPI

type OnTrackAPI =
  "on_track"
    :> Header "X-API-Key" Text
    :> ReqBody '[JSON] OnTrackTripReq
    :> Post '[JSON] OnTrackTripRes

onTrackTripAPI :: Proxy OnTrackAPI
onTrackTripAPI = Proxy

onTrackTrip :: Text -> OnTrackTripReq -> (RequestInfo, ET.EulerClient AckResponse)
onTrackTrip callbackApiKey = ET.client (withClientTracing onTrackTripAPI) (Just callbackApiKey)

type OnConfirmAPI =
  "on_confirm"
    :> Header "X-API-Key" Text
    :> ReqBody '[JSON] OnConfirmReq
    :> Post '[JSON] OnConfirmRes

onConfirmAPI :: Proxy OnConfirmAPI
onConfirmAPI = Proxy

onConfirm :: Text -> OnConfirmReq -> (RequestInfo, ET.EulerClient AckResponse)
onConfirm callbackApiKey = ET.client (withClientTracing onConfirmAPI) (Just callbackApiKey)

type OnCancelAPI =
  "on_cancel"
    :> ReqBody '[JSON] OnCancelReq
    :> Post '[JSON] OnCancelRes

cancelAPI :: Proxy OnCancelAPI
cancelAPI = Proxy

onCancel :: OnCancelReq -> (RequestInfo, ET.EulerClient AckResponse)
onCancel = ET.client $ withClientTracing cancelAPI

type OnStatusAPI =
  "on_status"
    :> ReqBody '[JSON] OnStatusReq
    :> Post '[JSON] OnStatusRes

statusAPI :: Proxy OnStatusAPI
statusAPI = Proxy

onStatus :: OnStatusReq -> (RequestInfo, ET.EulerClient AckResponse)
onStatus = ET.client $ withClientTracing statusAPI

type CallAPI =
  "call"
    :> "to_customer"
    :> ReqBody '[JSON] CallReq
    :> Post '[JSON] CallRes

callsAPI :: Proxy CallAPI
callsAPI = Proxy

initiateCall :: CallReq -> (RequestInfo, ET.EulerClient AckResponse)
initiateCall = ET.client $ withClientTracing callsAPI

type UpdateAPI =
  "on_update"
    :> Header "X-API-Key" Text
    :> ReqBody '[JSON] OnUpdateReq
    :> Post '[JSON] OnUpdateRes

updateAPI :: Proxy UpdateAPI
updateAPI = Proxy

onUpdate :: Text -> OnUpdateReq -> (RequestInfo, ET.EulerClient AckResponse)
onUpdate callbackApiKey = ET.client (withClientTracing updateAPI) (Just callbackApiKey)
