module Beckn.Types.Core.Cabs.API.Cancel where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Cabs.Cancel
import Beckn.Types.Core.ReqTypes (BecknReq)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type CancelReq = BecknReq CancelMessage

type CancelAPI =
  "cancel"
    :> ReqBody '[JSON] CancelReq
    :> Post '[JSON] AckResponse

cancelAPI :: Proxy CancelAPI
cancelAPI = Proxy
