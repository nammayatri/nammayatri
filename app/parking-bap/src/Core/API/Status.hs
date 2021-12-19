module Core.API.Status where

import Beckn.Prelude
import Beckn.Types.Core.Ack (AckResponse)
import Core.API.Types (BecknReq)
import Core.Status
import Servant (JSON, Post, ReqBody, (:>))

type StatusAPI =
  "status"
    :> ReqBody '[JSON] (BecknReq StatusMessage)
    :> Post '[JSON] AckResponse

statusAPI :: Proxy StatusAPI
statusAPI = Proxy
