module API.Beckn.FRFSSeller.Confirm (API, handler) where

import qualified API.Beckn.FRFSSeller.Handler as H
import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified Domain.Action.Beckn.FRFSSeller.Confirm as DConfirm
import Environment
import Kernel.Prelude
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth

type API = Spec.ConfirmAPI

handler :: Text -> SignatureAuthResult -> FlowServer API
handler = confirm'

confirm' :: Text -> SignatureAuthResult -> Spec.ConfirmReq -> FlowHandler Spec.AckResponse
confirm' operator _authResult req =
  withFlowHandlerAPI $
    H.acceptOnce
      operator
      "confirm"
      req.confirmReqContext.contextTransactionId
      req.confirmReqContext.contextMessageId
      (DConfirm.handleConfirm operator req)
