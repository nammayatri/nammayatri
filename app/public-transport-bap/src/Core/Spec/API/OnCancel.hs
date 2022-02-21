module Core.Spec.API.OnCancel where

import Beckn.Prelude
import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes
import Core.Spec.OnCancel (OnCancelMessage)
import Servant (JSON, Post, ReqBody, (:>))

type OnCancelAPI =
  "on_cancel"
    :> ReqBody '[JSON] (BecknCallbackReq OnCancelMessage)
    :> Post '[JSON] AckResponse

onCancelAPI :: Proxy OnCancelAPI
onCancelAPI = Proxy
