module API.Beckn.Search (API, handler) where

import Beckn.Prelude
import qualified Beckn.Storage.Hedis as Redis
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Taxi.API.Search as Search
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.OnSearch as ACL
import qualified Core.ACL.Search as ACL
import qualified Core.Beckn as CallBAP
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Merchant as DM
import Environment
import Servant

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth "Authorization"
    :> SignatureAuth "X-Gateway-Authorization"
    :> Search.SearchAPI

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
    logTagInfo "Search API Flow" "Reached"
    dSearchReq <- ACL.buildSearchReq subscriber req
    Redis.whenWithLockRedis (searchLockKey dSearchReq.messageId) 60 $ do
      dSearchRes <- DSearch.handler transporterId dSearchReq
      let context = req.context
      let callbackUrl = gateway.subscriber_url
      void $
        CallBAP.withCallback dSearchRes.provider Context.SEARCH OnSearch.onSearchAPI context callbackUrl $ do
          pure $ ACL.mkOnSearchMessage dSearchRes
    pure Ack

searchLockKey :: Text -> Text
searchLockKey id = "Driver:Search:MessageId-" <> id
