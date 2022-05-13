module Product.Search where

import App.Types
import Beckn.Types.Common hiding (id)
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import Beckn.Types.Id
import Beckn.Utils.Logging
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.Metro.Search as MetroACL
import qualified Core.ACL.OnSearch as TaxiACL
import qualified Core.ACL.Search as TaxiACL
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import qualified Domain.Action.UI.Search.OneWay as DOneWaySearch
import qualified Domain.Types.Person as Person
import EulerHS.Prelude hiding (id, state)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Types.API.Search as API
import Utils.Common

search :: Id Person.Person -> API.SearchReq -> FlowHandler API.SearchRes
search personId req = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  (searchRes, dSearchReq) <- DOneWaySearch.search personId req
  fork "search cabs" . withRetry $ do
    becknTaxiReq <- TaxiACL.buildOneWaySearchReq dSearchReq
    void $ ExternalAPI.search becknTaxiReq
  fork "search metro" . withRetry $ do
    becknMetroReq <- MetroACL.buildSearchReq dSearchReq
    ExternalAPI.searchMetro becknMetroReq
  fork "search public-transport" $ DOneWaySearch.sendPublicTransportSearchRequest personId dSearchReq
  return searchRes

searchCb ::
  SignatureAuthResult ->
  SignatureAuthResult ->
  OnSearch.OnSearchReq ->
  FlowHandler AckResponse
searchCb _ _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnSearchReq <- TaxiACL.buildOnSearchReq req
  DOnSearch.searchCb req.context.transaction_id mbDOnSearchReq
  pure Ack
