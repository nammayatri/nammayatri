module Beckn.Types.Core.Cabs.API.Confirm where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Cabs.API.Types (BecknReq)
import Beckn.Types.Core.Cabs.Confirm
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type ConfirmReq = BecknReq ConfirmMessage

type ConfirmAPI =
  "confirm"
    :> ReqBody '[JSON] ConfirmReq
    :> Post '[JSON] AckResponse

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy