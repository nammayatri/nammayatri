module Beckn.Types.Core.Cabs.API.Rating where

import Beckn.Types.Core.Ack
import Beckn.Types.Core.Cabs.Rating (RatingMessage)
import Beckn.Types.Core.ReqTypes (BecknReq)
import EulerHS.Prelude hiding (id)
import Servant (JSON, Post, ReqBody, (:>))

type RatingReq = BecknReq RatingMessage

type RatingAPI =
  "rating"
    :> ReqBody '[JSON] RatingReq
    :> Post '[JSON] AckResponse

ratingAPI :: Proxy RatingAPI
ratingAPI = Proxy
