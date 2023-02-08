module Core.Spec.API.OnConfirm where

import Core.Spec.OnConfirm
import Kernel.Types.Beckn.Ack
import Kernel.Types.Beckn.ReqTypes
import Servant

type OnConfirmAPI =
  "on_confirm"
    :> ReqBody '[JSON] (BecknCallbackReq OnConfirmMessage)
    :> Post '[JSON] AckResponse
