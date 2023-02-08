module Beckn.Types.Core.Taxi.API.Cancel where

import Beckn.Types.Core.Taxi.Cancel.Req
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknReq)
import Servant (JSON, Post, ReqBody, (:>))

type CancelReq = BecknReq CancelMessage

type CancelRes = AckResponse

type CancelAPI =
  "cancel"
    :> ReqBody '[JSON] CancelReq
    :> Post '[JSON] CancelRes

cancelAPI :: Proxy CancelAPI
cancelAPI = Proxy
