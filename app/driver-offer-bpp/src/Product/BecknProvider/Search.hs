module Product.BecknProvider.Search (search) where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Queries.BecknRequest as QBR
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Taxi.API.Search as Search
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.OnSearch as ACL
import qualified Core.ACL.Search as ACL
import Data.Aeson (encode)
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Organization as Org
import Environment
import qualified ExternalAPI.Flow as ExternalAPI
import qualified SharedLogic.Transporter as Shared
import Utils.Common

search ::
  Id Org.Organization ->
  SignatureAuthResult ->
  SignatureAuthResult ->
  Search.SearchReq ->
  FlowHandler AckResponse
search transporterId (SignatureAuthResult signPayload subscriber) (SignatureAuthResult _ gateway) req =
  withFlowHandlerAPI . withTransactionIdLogTag req $ do
    logTagInfo "Search API Flow" "Reached"
    Esq.runTransaction $
      QBR.logBecknRequest (show $ encode req) (show $ signPayload.signature)
    dSearchReq <- ACL.buildSearchReq subscriber req
    transporter <- Shared.findTransporter transporterId
    dSearchRes <- DSearch.handler transporter dSearchReq
    let context = req.context
    let callbackUrl = gateway.subscriber_url
    ExternalAPI.withCallback' withRetry transporter Context.SEARCH OnSearch.onSearchAPI context callbackUrl $ do
      pure $ ACL.mkOnSearchMessage dSearchRes
