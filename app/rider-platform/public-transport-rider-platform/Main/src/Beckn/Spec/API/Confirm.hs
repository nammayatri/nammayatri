module Beckn.Spec.API.Confirm where

import Beckn.Spec.Confirm
import Kernel.Types.Beckn.Ack
import Kernel.Types.Beckn.ReqTypes
import Servant

type ConfirmAPI =
  "confirm" :> ReqBody '[JSON] (BecknReq ConfirmMessage) :> Post '[JSON] AckResponse

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy
