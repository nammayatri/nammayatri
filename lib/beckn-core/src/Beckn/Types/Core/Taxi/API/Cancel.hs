module Beckn.Types.Core.Taxi.API.Cancel where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes (BecknReq)
import Beckn.Types.Core.Taxi.Cancel
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type CancelReq = BecknReq CancelMessage

type CancelAPI =
  "cancel"
    :> ReqBody '[JSON] CancelReq
    :> Post '[JSON] AckResponse

cancelAPI :: Proxy CancelAPI
cancelAPI = Proxy
