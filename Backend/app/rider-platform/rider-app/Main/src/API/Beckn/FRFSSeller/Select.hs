module API.Beckn.FRFSSeller.Select (API, handler) where

import qualified API.Beckn.FRFSSeller.Handler as H
import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified Domain.Action.Beckn.FRFSSeller.Select as DSelect
import Environment
import Kernel.Prelude
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth

type API = Spec.SelectAPI

handler :: Text -> SignatureAuthResult -> FlowServer API
handler = select

select :: Text -> SignatureAuthResult -> Spec.SelectReq -> FlowHandler Spec.AckResponse
select operator _authResult req =
  withFlowHandlerAPI $
    H.acceptOnce
      operator
      "select"
      req.selectReqContext.contextTransactionId
      req.selectReqContext.contextMessageId
      (DSelect.handleSelect operator req)
