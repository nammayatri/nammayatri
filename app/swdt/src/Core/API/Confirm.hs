module Core.API.Confirm where

-- import Beckn.Prelude
import Beckn.Types.Core.Ack (AckResponse)
import Core.ReqTypes
import Core.Confirm (ConfirmMessage)
import Servant 

type ConfirmAPI =
  "confirm"
    :> ReqBody '[JSON] (BecknReq ConfirmMessage)
    :> Post '[JSON] AckResponse

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy