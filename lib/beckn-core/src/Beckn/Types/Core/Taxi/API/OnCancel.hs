module Beckn.Types.Core.Taxi.API.OnCancel where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes (BecknCallbackReq)
import Beckn.Types.Core.Taxi.OnCancel
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type OnCancelReq = BecknCallbackReq OnCancelMessage

type OnCancelRes = AckResponse

type OnCancelAPI =
  "on_cancel"
    :> ReqBody '[JSON] OnCancelReq
    :> Post '[JSON] OnCancelRes

onCancelAPI :: Proxy OnCancelAPI
onCancelAPI = Proxy
