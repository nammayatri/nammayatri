module Core.API.OnConfirm where

import Beckn.Prelude
import Beckn.Types.Core.Ack (AckResponse)
import Core.API.Types (BecknCallbackReq)
import Core.OnConfirm
import Servant (JSON, Post, ReqBody, (:>))

type OnConfirmAPI =
  "on_confirm"
    :> ReqBody '[JSON] (BecknCallbackReq OnConfirmMessage)
    :> Post '[JSON] AckResponse

onConfirmAPI :: Proxy OnConfirmAPI
onConfirmAPI = Proxy
