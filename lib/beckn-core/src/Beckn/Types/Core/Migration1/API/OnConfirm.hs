module Beckn.Types.Core.Migration1.API.OnConfirm where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Migration1.API.Types (BecknCallbackReq)
import Beckn.Types.Core.Migration1.OnConfirm (OnConfirmMessage)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type OnConfirmReq = BecknCallbackReq OnConfirmMessage

type OnConfirmAPI =
  "on_confirm"
    :> ReqBody '[JSON] OnConfirmReq
    :> Post '[JSON] AckResponse

onConfirmAPI :: Proxy OnConfirmAPI
onConfirmAPI = Proxy
