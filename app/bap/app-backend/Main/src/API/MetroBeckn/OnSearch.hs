module API.MetroBeckn.OnSearch (API, handler) where

import Beckn.Types.Core.Metro.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Metro.API.OnSearch as Metro
import qualified Core.ACL.Metro.OnSearch as MetroACL
import Environment
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import qualified SharedLogic.MetroOffer as MetroOffers

type API =
  SignatureAuth "X-Gateway-Authorization"
    :> Metro.OnSearchAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onSearch

onSearch ::
  SignatureAuthResult ->
  SignatureAuthResult ->
  OnSearch.OnSearchReq ->
  FlowHandler AckResponse
onSearch _ _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  let searchReqId = Id req.context.message_id
  mbMetroOffers <- MetroACL.buildMetroOffers req
  whenJust mbMetroOffers $ MetroOffers.cacheMetroOffers searchReqId
  pure Ack
