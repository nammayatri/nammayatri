module API.Beckn.FRFSSeller.Cancel (API, handler) where

import qualified API.Beckn.FRFSSeller.Handler as H
import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified Domain.Action.Beckn.FRFSSeller.Cancel as DCancel
import Environment
import Kernel.Prelude
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth

type API = Spec.CancelAPI

handler :: Text -> SignatureAuthResult -> FlowServer API
handler = cancel

cancel :: Text -> SignatureAuthResult -> Spec.CancelReq -> FlowHandler Spec.AckResponse
cancel operator _authResult req =
  withFlowHandlerAPI $
    H.acceptOnce
      operator
      "cancel"
      req.cancelReqContext.contextTransactionId
      req.cancelReqContext.contextMessageId
      (DCancel.handleCancel operator req)
