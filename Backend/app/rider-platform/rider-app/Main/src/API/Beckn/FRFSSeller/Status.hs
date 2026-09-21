module API.Beckn.FRFSSeller.Status (API, handler) where

import qualified API.Beckn.FRFSSeller.Handler as H
import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified Domain.Action.Beckn.FRFSSeller.Status as DStatus
import Environment
import Kernel.Prelude
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth

type API = Spec.StatusAPI

handler :: Text -> SignatureAuthResult -> FlowServer API
handler = status

status :: Text -> SignatureAuthResult -> Spec.StatusReq -> FlowHandler Spec.AckResponse
status operator _authResult req =
  withFlowHandlerAPI $
    H.acceptOnce
      operator
      "status"
      req.statusReqContext.contextTransactionId
      req.statusReqContext.contextMessageId
      (DStatus.handleStatus operator req)
