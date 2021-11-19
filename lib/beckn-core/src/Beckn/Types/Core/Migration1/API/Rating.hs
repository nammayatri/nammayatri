module Beckn.Types.Core.Migration1.API.Rating where

import Beckn.Types.Core.Ack
import Beckn.Types.Core.Migration1.API.Types (BecknReq)
import Beckn.Types.Core.Migration1.Rating (RatingMessage)
import EulerHS.Prelude hiding (id)
import Servant (JSON, Post, ReqBody, (:>))

type RatingReq = BecknReq RatingMessage

type RatingAPI =
  "rating"
    :> ReqBody '[JSON] RatingReq
    :> Post '[JSON] AckResponse

ratingAPI :: Proxy RatingAPI
ratingAPI = Proxy
