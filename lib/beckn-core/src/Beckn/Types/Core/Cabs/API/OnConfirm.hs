module Beckn.Types.Core.Cabs.API.OnConfirm where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Cabs.OnConfirm (OnConfirmMessage)
import Beckn.Types.Core.ReqTypes (BecknCallbackReq)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type OnConfirmReq = BecknCallbackReq OnConfirmMessage

type OnConfirmAPI =
  "on_confirm"
    :> ReqBody '[JSON] OnConfirmReq
    :> Post '[JSON] AckResponse

onConfirmAPI :: Proxy OnConfirmAPI
onConfirmAPI = Proxy
