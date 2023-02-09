module Beckn.Spec.API.Status where

import Beckn.Spec.Status
import Kernel.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes
import Servant (JSON, Post, ReqBody, (:>))

type StatusAPI =
  "status"
    :> ReqBody '[JSON] (BecknReq StatusMessage)
    :> Post '[JSON] AckResponse

statusAPI :: Proxy StatusAPI
statusAPI = Proxy
