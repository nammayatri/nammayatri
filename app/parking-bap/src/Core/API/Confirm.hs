module Core.API.Confirm where

import Beckn.Prelude
import Beckn.Types.Core.Ack (AckResponse)
import Core.API.Types (BecknReq)
import Core.Confirm (ConfirmMessage)
import Servant (JSON, Post, ReqBody, (:>))

type ConfirmAPI =
  "confirm"
    :> ReqBody '[JSON] (BecknReq ConfirmMessage)
    :> Post '[JSON] AckResponse

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy
