module Core.Spec.API.OnConfirm where

import Beckn.Types.Core.Ack
import Beckn.Types.Core.ReqTypes
import Core.Spec.OnConfirm
import Servant

type OnConfirmAPI =
  "on_confirm"
    :> ReqBody '[JSON] (BecknCallbackReq OnConfirmMessage)
    :> Post '[JSON] AckResponse
