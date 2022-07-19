module Beckn.Types.Core.Taxi.API.Track where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes (BecknReq)
import Beckn.Types.Core.Taxi.Track
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type TrackReq = BecknReq TrackMessage

type TrackRes = AckResponse

type TrackAPI =
  "track"
    :> ReqBody '[JSON] TrackReq
    :> Post '[JSON] TrackRes

trackAPI :: Proxy TrackAPI
trackAPI = Proxy
