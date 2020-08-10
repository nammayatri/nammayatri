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

type TrackAPI =
  "on_track"
    :> ReqBody '[JSON] OnTrackTripReq
    :> Post '[JSON] OnTrackTripRes

trackTripAPI :: Proxy TrackAPI
trackTripAPI = Proxy

onTrackTrip :: OnTrackTripReq -> (RequestInfo, ET.EulerClient AckResponse)
onTrackTrip = ET.client $ withClientTracing trackTripAPI

type ConfirmAPI =
  "on_confirm"
    :> ReqBody '[JSON] OnConfirmReq
    :> Post '[JSON] OnConfirmRes

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy

onConfirm :: OnConfirmReq -> (RequestInfo, ET.EulerClient AckResponse)
onConfirm = ET.client $ withClientTracing confirmAPI

type CancelAPI =
  "on_cancel"
    :> ReqBody '[JSON] OnCancelReq
    :> Post '[JSON] OnCancelRes

cancelAPI :: Proxy CancelAPI
cancelAPI = Proxy

onCancel :: OnCancelReq -> (RequestInfo, ET.EulerClient AckResponse)
onCancel = ET.client $ withClientTracing cancelAPI

type StatusAPI =
  "on_status"
    :> ReqBody '[JSON] OnStatusReq
    :> Post '[JSON] OnStatusRes

statusAPI :: Proxy StatusAPI
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
    :> ReqBody '[JSON] OnUpdateReq
    :> Post '[JSON] OnUpdateRes

updateAPI :: Proxy UpdateAPI
updateAPI = Proxy

onUpdate :: OnUpdateReq -> (RequestInfo, ET.EulerClient AckResponse)
onUpdate = ET.client $ withClientTracing updateAPI
