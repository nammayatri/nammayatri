module API.Beckn.FRFSSeller.Search (API, handler) where

import qualified API.Beckn.FRFSSeller.Handler as H
import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified Domain.Action.Beckn.FRFSSeller.Search as DSearch
import Environment
import Kernel.Prelude
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth

type API = Spec.SearchAPI

handler :: Text -> SignatureAuthResult -> FlowServer API
handler = search

search :: Text -> SignatureAuthResult -> Spec.SearchReq -> FlowHandler Spec.AckResponse
search operator _authResult req =
  withFlowHandlerAPI $
    H.acceptOnce
      operator
      "search"
      req.searchReqContext.contextTransactionId
      req.searchReqContext.contextMessageId
      (DSearch.handleSearch operator req)
