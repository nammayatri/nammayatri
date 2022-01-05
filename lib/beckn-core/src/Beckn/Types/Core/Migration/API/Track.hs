module Beckn.Types.Core.Migration.API.Track where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Migration.Tracking (Tracking)
import Beckn.Types.Core.ReqTypes (BecknCallbackReq, BecknReq)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))
import Servant.Client (BaseUrl)

type TrackAPI =
  "track"
    :> ReqBody '[JSON] (BecknReq TrackInfo)
    :> Post '[JSON] AckResponse

trackAPI :: Proxy TrackAPI
trackAPI = Proxy

data TrackInfo = TrackInfo
  { order_id :: Text,
    callback_url :: Maybe BaseUrl
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type OnTrackAPI =
  "on_track"
    :> ReqBody '[JSON] (BecknCallbackReq OnTrackInfo)
    :> Post '[JSON] AckResponse

onTrackAPI :: Proxy OnTrackAPI
onTrackAPI = Proxy

newtype OnTrackInfo = OnTrackInfo
  { tracking :: Tracking
  }
  deriving (Generic, Show, FromJSON, ToJSON)
