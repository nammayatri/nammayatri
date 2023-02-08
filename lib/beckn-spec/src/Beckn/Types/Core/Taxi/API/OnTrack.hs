module Beckn.Types.Core.Taxi.API.OnTrack where

import Beckn.Types.Core.Taxi.OnTrack
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknCallbackReq)
import Servant (JSON, Post, ReqBody, (:>))

type OnTrackReq = BecknCallbackReq OnTrackMessage

type OnTrackRes = AckResponse

type OnTrackAPI =
  "on_track"
    :> ReqBody '[JSON] OnTrackReq
    :> Post '[JSON] OnTrackRes

onTrackAPI :: Proxy OnTrackAPI
onTrackAPI = Proxy
