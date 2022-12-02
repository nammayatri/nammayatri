module Core.Spec.API.Confirm where

import Beckn.Types.Core.Ack
import Beckn.Types.Core.ReqTypes
import Core.Spec.Confirm
import Servant

type ConfirmAPI =
  "confirm" :> ReqBody '[JSON] (BecknReq ConfirmMessage) :> Post '[JSON] AckResponse

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy
