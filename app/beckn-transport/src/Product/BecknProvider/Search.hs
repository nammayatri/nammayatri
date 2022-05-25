module Product.BecknProvider.Search (search) where

import App.Types
import qualified Beckn.Storage.Esqueleto as Esq
import qualified Beckn.Storage.Queries.BecknRequest as QBR
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Taxi.API.Search as Search
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.OnSearch as ACL
import qualified Core.ACL.Search as ACL
import Data.Aeson (encode)
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
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
search transporterId (SignatureAuthResult signPayload subscriber) (SignatureAuthResult _ gateway) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    Esq.runTransaction $
      QBR.logBecknRequest (show $ encode req) (show $ signPayload.signature)
    dSearchReq <- ACL.buildSearchReq subscriber req
    DSearch.DSearchRes {..} <- DSearch.search transporterId dSearchReq
    let context = req.context
    let callbackUrl = gateway.subscriber_url
    ExternalAPI.withCallback' withRetry transporter Context.SEARCH OnSearch.onSearchAPI context callbackUrl $ do
      dOnSearchRes <- DOnSearch.onSearch (DOnSearch.DOnSearchReq {..})
      pure $ ACL.mkOnSearchMessage dOnSearchRes
