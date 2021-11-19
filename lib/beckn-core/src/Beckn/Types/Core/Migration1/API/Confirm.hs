module Beckn.Types.Core.Migration1.API.Confirm where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Migration1.API.Types (BecknReq)
import Beckn.Types.Core.Migration1.Confirm
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type ConfirmReq = BecknReq ConfirmMessage

type ConfirmAPI =
  "confirm"
    :> ReqBody '[JSON] ConfirmReq
    :> Post '[JSON] AckResponse

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy