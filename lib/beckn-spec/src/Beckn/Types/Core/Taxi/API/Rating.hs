module Beckn.Types.Core.Taxi.API.Rating where

import Beckn.Types.Core.Taxi.Rating (RatingMessage)
import EulerHS.Prelude hiding (id)
import Kernel.Types.Beckn.Ack
import Kernel.Types.Beckn.ReqTypes (BecknReq)
import Servant (JSON, Post, ReqBody, (:>))

type RatingReq = BecknReq RatingMessage

type RatingRes = AckResponse

type RatingAPI =
  "rating"
    :> ReqBody '[JSON] RatingReq
    :> Post '[JSON] RatingRes

ratingAPI :: Proxy RatingAPI
ratingAPI = Proxy
