module Core.API.Status where

import Beckn.Prelude
import Beckn.Types.Core.Ack (AckResponse)
import Core.API.Types (BecknReq)
import Servant (JSON, Post, ReqBody, (:>))

type StatusAPI =
  "status"
    :> ReqBody '[JSON] (BecknReq Text)
    :> Post '[JSON] AckResponse

statusAPI :: Proxy StatusAPI
statusAPI = Proxy
