module Beckn.Types.Core.Taxi.API.Confirm where

import Beckn.Types.Core.Taxi.Confirm
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknReq)
import Servant (JSON, Post, ReqBody, (:>))

type ConfirmReq = BecknReq ConfirmMessage

type ConfirmRes = AckResponse

type ConfirmAPI =
  "confirm"
    :> ReqBody '[JSON] ConfirmReq
    :> Post '[JSON] ConfirmRes

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy
