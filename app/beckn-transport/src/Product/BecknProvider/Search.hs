module Product.BecknProvider.Search (search) where

import App.Types
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Migration.Context
import qualified Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Taxi.API.Search as Search
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.OnSearch as ACL
import qualified Core.ACL.Search as ACL
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Organization as Org
import EulerHS.Prelude hiding (state)
import qualified ExternalAPI.Flow as ExternalAPI
import Utils.Common

search ::
  Id Org.Organization ->
  SignatureAuthResult ->
  SignatureAuthResult ->
  Search.SearchReq ->
  FlowHandler AckResponse
search transporterId (SignatureAuthResult _ subscriber) (SignatureAuthResult _ gateway) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    dSearchReq <- ACL.buildSearchReq subscriber req
    transporter <- DSearch.findTransporter transporterId
    let context = req.context
    let callbackUrl = gateway.subscriber_url
    ExternalAPI.withCallback' withRetry transporter SEARCH OnSearch.onSearchAPI context callbackUrl $ do
      dOnSearchReq <- DSearch.handler transporter dSearchReq
      pure $ ACL.mkOnSearchMessage dOnSearchReq
