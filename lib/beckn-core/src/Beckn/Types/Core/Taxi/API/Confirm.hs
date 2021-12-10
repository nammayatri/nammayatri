module Beckn.Types.Core.Taxi.API.Confirm where

import Beckn.Types.Core.ReqTypes (BecknReq)
import Beckn.Types.Core.Taxi.Confirm.Req (ConfirmReqMessage)
import Beckn.Types.Core.Taxi.Confirm.Res (ConfirmResMessage)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type ConfirmReq = BecknReq ConfirmReqMessage

type ConfirmRes = ConfirmResMessage

type ConfirmAPI =
  "confirm"
    :> ReqBody '[JSON] ConfirmReq
    :> Post '[JSON] ConfirmRes

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy
