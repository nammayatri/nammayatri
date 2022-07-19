module Beckn.Types.Core.Taxi.API.OnConfirm where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes (BecknCallbackReq)
import Beckn.Types.Core.Taxi.OnConfirm (OnConfirmMessage)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type OnConfirmReq = BecknCallbackReq OnConfirmMessage

type OnConfirmRes = AckResponse

type OnConfirmAPI =
  "on_confirm"
    :> ReqBody '[JSON] OnConfirmReq
    :> Post '[JSON] OnConfirmRes

onConfirmAPI :: Proxy OnConfirmAPI
onConfirmAPI = Proxy
