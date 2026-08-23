module API.Beckn.FRFSSeller.Init (API, handler) where

import qualified API.Beckn.FRFSSeller.Handler as H
import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified Domain.Action.Beckn.FRFSSeller.Init as DInit
import Environment
import Kernel.Prelude
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth

type API = Spec.InitAPI

handler :: Text -> SignatureAuthResult -> FlowServer API
handler = init'

init' :: Text -> SignatureAuthResult -> Spec.InitReq -> FlowHandler Spec.AckResponse
init' operator _authResult req =
  withFlowHandlerAPI $
    H.acceptOnce
      operator
      "init"
      req.initReqContext.contextTransactionId
      req.initReqContext.contextMessageId
      (DInit.handleInit operator req)
