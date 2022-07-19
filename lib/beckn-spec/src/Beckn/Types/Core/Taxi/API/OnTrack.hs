module Beckn.Types.Core.Taxi.API.OnTrack where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes (BecknCallbackReq)
import Beckn.Types.Core.Taxi.OnTrack
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type OnTrackReq = BecknCallbackReq OnTrackMessage

type OnTrackRes = AckResponse

type OnTrackAPI =
  "on_track"
    :> ReqBody '[JSON] OnTrackReq
    :> Post '[JSON] OnTrackRes

onTrackAPI :: Proxy OnTrackAPI
onTrackAPI = Proxy
