module Beckn.Types.Core.Taxi.API.Confirm where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes (BecknReq)
import Beckn.Types.Core.Taxi.Confirm
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type ConfirmReq = BecknReq ConfirmMessage

type ConfirmRes = AckResponse

type ConfirmAPI =
  "confirm"
    :> ReqBody '[JSON] ConfirmReq
    :> Post '[JSON] ConfirmRes

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy
