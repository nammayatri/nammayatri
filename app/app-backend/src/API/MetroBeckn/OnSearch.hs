module API.MetroBeckn.OnSearch (API, handler) where

import Beckn.Prelude
import Beckn.Types.Core.Metro.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Metro.API.OnSearch as Metro
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.Metro.OnSearch as MetroACL
import Environment
import Servant hiding (throwError)
import qualified SharedLogic.MetroOffer as MetroOffers
import qualified Tools.Metrics as Metrics

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
  Metrics.finishSearchMetrics searchReqId.getId
  mbMetroOffers <- MetroACL.buildMetroOffers req
  whenJust mbMetroOffers $ MetroOffers.cacheMetroOffers searchReqId
  pure Ack
