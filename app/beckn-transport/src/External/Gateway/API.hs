module External.Gateway.API where

import Beckn.Types.API.Cancel
import Beckn.Types.API.Confirm as Confirm
import qualified Beckn.Types.API.Search as Search
import Beckn.Types.API.Status as Status
import Beckn.Types.API.Track
import Beckn.Types.API.Update as Update
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Servant

onSearch req =
  void $ ET.client Search.onSearchAPI "mobility-provider-key" req

type TrackAPI =
  "on_track"
    :> ReqBody '[JSON] OnTrackTripReq
    :> Post '[JSON] OnTrackTripRes

trackTripAPI :: Proxy TrackAPI
trackTripAPI = Proxy

onTrackTrip req =
  void $ ET.client trackTripAPI req

type ConfirmAPI =
  "on_confirm"
    :> ReqBody '[JSON] Confirm.OnConfirmReq
    :> Post '[JSON] Confirm.OnConfirmRes

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy

onConfirm req =
  void $ ET.client confirmAPI req

type CancelAPI =
  "on_cancel"
    :> ReqBody '[JSON] OnCancelReq
    :> Post '[JSON] OnCancelRes

cancelAPI :: Proxy CancelAPI
cancelAPI = Proxy

onCancel req =
  void $ ET.client cancelAPI req

type StatusAPI =
  "on_status"
    :> ReqBody '[JSON] Status.OnStatusReq
    :> Post '[JSON] Status.OnStatusRes

statusAPI :: Proxy StatusAPI
statusAPI = Proxy

onStatus req =
  void $ ET.client statusAPI req

type UpdateAPI =
  "on_update"
    :> ReqBody '[JSON] Update.OnUpdateReq
    :> Post '[JSON] Update.OnUpdateRes

updateAPI :: Proxy UpdateAPI
updateAPI = Proxy

onUpdate req =
  void $ ET.client updateAPI req
