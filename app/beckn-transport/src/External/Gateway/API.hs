module External.Gateway.API where

import Beckn.Types.API.Call
import Beckn.Types.API.Cancel
import Beckn.Types.API.Confirm
import Beckn.Types.API.Search
import Beckn.Types.API.Status
import Beckn.Types.API.Track
import Beckn.Types.API.Update
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant

onSearch :: Text -> OnSearchReq -> ET.EulerClient ()
onSearch apiKey req =
  void $ ET.client onSearchAPI apiKey req

type TrackAPI =
  "on_track"
    :> ReqBody '[JSON] OnTrackTripReq
    :> Post '[JSON] OnTrackTripRes

trackTripAPI :: Proxy TrackAPI
trackTripAPI = Proxy

onTrackTrip :: OnTrackTripReq -> ET.EulerClient ()
onTrackTrip = void . ET.client trackTripAPI

type ConfirmAPI =
  "on_confirm"
    :> ReqBody '[JSON] OnConfirmReq
    :> Post '[JSON] OnConfirmRes

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy

onConfirm :: OnConfirmReq -> ET.EulerClient ()
onConfirm = void . ET.client confirmAPI

type CancelAPI =
  "on_cancel"
    :> ReqBody '[JSON] OnCancelReq
    :> Post '[JSON] OnCancelRes

cancelAPI :: Proxy CancelAPI
cancelAPI = Proxy

onCancel :: OnCancelReq -> ET.EulerClient ()
onCancel = void . ET.client cancelAPI

type StatusAPI =
  "on_status"
    :> ReqBody '[JSON] OnStatusReq
    :> Post '[JSON] OnStatusRes

statusAPI :: Proxy StatusAPI
statusAPI = Proxy

onStatus :: OnStatusReq -> ET.EulerClient ()
onStatus = void . ET.client statusAPI

type CallAPI =
  "call"
    :> "to_customer"
    :> ReqBody '[JSON] CallReq
    :> Post '[JSON] CallRes

callAPI :: Proxy CallAPI
callAPI = Proxy

initiateCall :: CallReq -> ET.EulerClient ()
initiateCall = void . ET.client callAPI

type UpdateAPI =
  "on_update"
    :> ReqBody '[JSON] OnUpdateReq
    :> Post '[JSON] OnUpdateRes

updateAPI :: Proxy UpdateAPI
updateAPI = Proxy

onUpdate :: OnUpdateReq -> ET.EulerClient ()
onUpdate = void . ET.client updateAPI
