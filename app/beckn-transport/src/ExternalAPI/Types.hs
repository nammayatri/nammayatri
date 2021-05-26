module ExternalAPI.Types where

import Beckn.Types.Core.API.Call
import Beckn.Types.Core.API.Cancel
import Beckn.Types.Core.API.Confirm
import Beckn.Types.Core.API.Search (OnSearchReq, OnSearchRes, nsdlOnSearchAPI)
import Beckn.Types.Core.API.Status
import Beckn.Types.Core.API.Track
import Beckn.Types.Core.API.Update
import Beckn.Types.Core.Ack
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant

type OnSearchAPI =
  "on_search"
    :> ReqBody '[JSON] OnSearchReq
    :> Post '[JSON] OnSearchRes

onSearchAPI :: Proxy OnSearchAPI
onSearchAPI = Proxy

onSearch :: OnSearchReq -> ET.EulerClient AckResponse
onSearch = ET.client onSearchAPI

nsdlOnSearch :: OnSearchReq -> ET.EulerClient AckResponse
nsdlOnSearch = ET.client nsdlOnSearchAPI

type OnTrackAPI =
  "on_track"
    :> ReqBody '[JSON] OnTrackTripReq
    :> Post '[JSON] OnTrackTripRes

onTrackTripAPI :: Proxy OnTrackAPI
onTrackTripAPI = Proxy

onTrackTrip :: OnTrackTripReq -> ET.EulerClient AckResponse
onTrackTrip = ET.client onTrackTripAPI

type OnConfirmAPI =
  "on_confirm"
    :> ReqBody '[JSON] OnConfirmReq
    :> Post '[JSON] OnConfirmRes

onConfirmAPI :: Proxy OnConfirmAPI
onConfirmAPI = Proxy

onConfirm :: OnConfirmReq -> ET.EulerClient AckResponse
onConfirm = ET.client onConfirmAPI

type OnCancelAPI =
  "on_cancel"
    :> ReqBody '[JSON] OnCancelReq
    :> Post '[JSON] OnCancelRes

cancelAPI :: Proxy OnCancelAPI
cancelAPI = Proxy

onCancel :: OnCancelReq -> ET.EulerClient AckResponse
onCancel = ET.client cancelAPI

type OnStatusAPI =
  "on_status"
    :> ReqBody '[JSON] OnStatusReq
    :> Post '[JSON] OnStatusRes

statusAPI :: Proxy OnStatusAPI
statusAPI = Proxy

onStatus :: OnStatusReq -> ET.EulerClient AckResponse
onStatus = ET.client statusAPI

type CallAPI =
  "call"
    :> "to_customer"
    :> ReqBody '[JSON] CallReq
    :> Post '[JSON] CallRes

callsAPI :: Proxy CallAPI
callsAPI = Proxy

initiateCall :: CallReq -> ET.EulerClient AckResponse
initiateCall = ET.client callsAPI

type UpdateAPI =
  "on_update"
    :> ReqBody '[JSON] OnUpdateReq
    :> Post '[JSON] OnUpdateRes

updateAPI :: Proxy UpdateAPI
updateAPI = Proxy

onUpdate :: OnUpdateReq -> ET.EulerClient AckResponse
onUpdate = ET.client updateAPI
