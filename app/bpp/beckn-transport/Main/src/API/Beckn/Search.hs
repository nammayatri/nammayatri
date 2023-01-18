module API.Beckn.Search (API, handler) where

import qualified Beckn.Storage.Hedis as Redis
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Taxi.API.Search as API
import qualified Beckn.Types.Core.Taxi.API.Search as Search
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.OnSearch as ACL
import qualified Core.ACL.Search as ACL
import Core.Beckn (withCallback')
import qualified Domain.Action.Beckn.OnSearch as DOnSearch
import qualified Domain.Action.Beckn.Search as DSearch
import Domain.Types.Merchant (Merchant)
import qualified Domain.Types.Merchant as DM
import Environment
import EulerHS.Prelude hiding (id)
import Servant

type API =
  Capture "merchantId" (Id Merchant)
    :> SignatureAuth "Authorization"
    :> SignatureAuth "X-Gateway-Authorization"
    :> API.SearchAPI

handler :: FlowServer API
handler = search

search ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  SignatureAuthResult ->
  Search.SearchReq ->
  FlowHandler AckResponse
search transporterId (SignatureAuthResult _ subscriber _) (SignatureAuthResult _ gateway _) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    dSearchReq <- ACL.buildSearchReq subscriber req
    Redis.whenWithLockRedis (searchLockKey dSearchReq.messageId) 60 $ do
      DSearch.DSearchRes {..} <- DSearch.search transporterId dSearchReq
      let context = req.context
      let callbackUrl = gateway.subscriber_url
      let transactionId = dSearchReq.transactionId
      void $
        withCallback' withShortRetry transporter Context.SEARCH OnSearch.onSearchAPI context callbackUrl $ do
          dOnSearchRes <- DOnSearch.onSearch (DOnSearch.DOnSearchReq {..})
          pure $ ACL.mkOnSearchMessage dOnSearchRes
    return Ack

searchLockKey :: Text -> Text
searchLockKey id = "Driver:Search:MessageId-" <> id
