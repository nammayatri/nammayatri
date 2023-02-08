module Beckn.Types.Core.Taxi.API.Track where

import Beckn.Types.Core.Taxi.Track
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknReq)
import Servant (JSON, Post, ReqBody, (:>))

type TrackReq = BecknReq TrackMessage

type TrackRes = AckResponse

type TrackAPI =
  "track"
    :> ReqBody '[JSON] TrackReq
    :> Post '[JSON] TrackRes

trackAPI :: Proxy TrackAPI
trackAPI = Proxy
