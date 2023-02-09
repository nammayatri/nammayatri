module API.Beckn.Search (API, handler) where

import qualified Beckn.ACL.OnSearch as ACL
import qualified Beckn.ACL.Search as ACL
import qualified Beckn.Core as CallBAP
import qualified Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Taxi.API.Search as Search
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
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
