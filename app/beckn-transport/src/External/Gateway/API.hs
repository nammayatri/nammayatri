module External.Gateway.API where

import Beckn.Types.API.Confirm as Confirm
import Beckn.Types.API.Search
import Beckn.Types.API.Cancel
import Beckn.Types.API.Status as Status
import Beckn.Types.API.Track
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import External.Gateway.Types
import Servant
import Servant.API.ContentTypes
import Servant.Client

type SearchAPI =
  "on_search"
    :> "services"
    :> ReqBody '[JSON] OnSearchReq
    :> Post '[JSON] OnSearchRes

searchAPI :: Proxy SearchAPI
searchAPI = Proxy

onSearch req =
  void $ ET.client searchAPI req

type TrackAPI =
  "on_track"
    :> "trip"
    :> ReqBody '[JSON] OnTrackTripReq
    :> Post '[JSON] OnTrackTripRes

trackTripAPI :: Proxy TrackAPI
trackTripAPI = Proxy

onTrackTrip req =
  void $ ET.client trackTripAPI req

type ConfirmAPI =
  "on_confirm"
    :> "services"
    :> ReqBody '[JSON] Confirm.OnConfirmReq
    :> Post '[JSON] Confirm.OnConfirmRes

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy

onConfirm req =
  void $ ET.client confirmAPI req

type CancelAPI =
  "on_cancel"
    :> "services"
    :> ReqBody '[JSON] OnCancelReq
    :> Post '[JSON] OnCancelRes

cancelAPI :: Proxy CancelAPI
cancelAPI = Proxy

onCancel req =
  void $ ET.client cancelAPI req

type StatusAPI =
  "on_status"
    :> "services"
    :> ReqBody '[JSON] Status.OnStatusReq
    :> Post '[JSON] Status.OnStatusRes

statusAPI :: Proxy StatusAPI
statusAPI = Proxy

onStatus req =
  void $ ET.client statusAPI req
