module External.Gateway.API where

import Beckn.Types.API.Search
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
