module Beckn.Types.Core.Taxi.API.OnConfirm where

import Beckn.Types.Core.Taxi.OnConfirm (OnConfirmMessage)
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes (BecknCallbackReq)
import Servant (JSON, Post, ReqBody, (:>))

type OnConfirmReq = BecknCallbackReq OnConfirmMessage

type OnConfirmRes = AckResponse

type OnConfirmAPI =
  "on_confirm"
    :> ReqBody '[JSON] OnConfirmReq
    :> Post '[JSON] OnConfirmRes

onConfirmAPI :: Proxy OnConfirmAPI
onConfirmAPI = Proxy
