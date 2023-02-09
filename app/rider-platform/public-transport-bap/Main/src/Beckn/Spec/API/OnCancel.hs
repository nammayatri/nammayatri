module Beckn.Spec.API.OnCancel where

import Beckn.Spec.OnCancel (OnCancelMessage)
import Kernel.Prelude
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes
import Servant (JSON, Post, ReqBody, (:>))

type OnCancelAPI =
  "on_cancel"
    :> ReqBody '[JSON] (BecknCallbackReq OnCancelMessage)
    :> Post '[JSON] AckResponse

onCancelAPI :: Proxy OnCancelAPI
onCancelAPI = Proxy
