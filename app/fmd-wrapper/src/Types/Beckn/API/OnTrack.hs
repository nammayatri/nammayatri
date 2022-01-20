module Types.Beckn.API.OnTrack (module Types.Beckn.API.OnTrack, module Reexport) where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))
import Types.Beckn.Tracking as Reexport (Tracking (..), TrackingStatus (..))

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
