module API.Beckn.Search (API, handler) where

import Beckn.Prelude
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Taxi.API.Search as Search
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.OnSearch as ACL
import qualified Core.ACL.Search as ACL
import qualified Core.Beckn as CallBAP
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Organization as Org
import Environment
import Servant
import qualified SharedLogic.Transporter as Shared
import Utils.Common

type API =
  Capture "orgId" (Id Org.Organization)
    :> SignatureAuth "Authorization"
    :> SignatureAuth "X-Gateway-Authorization"
    :> Search.SearchAPI

handler :: FlowServer API
handler = search

search ::
  Id Org.Organization ->
  SignatureAuthResult ->
  SignatureAuthResult ->
  Search.SearchReq ->
  FlowHandler AckResponse
search transporterId (SignatureAuthResult _ subscriber _) (SignatureAuthResult _ gateway _) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    logTagInfo "Search API Flow" "Reached"
    dSearchReq <- ACL.buildSearchReq subscriber req
    transporter <- Shared.findTransporter transporterId
    dSearchRes <- DSearch.handler transporter dSearchReq
    let context = req.context
    let callbackUrl = gateway.subscriber_url
    CallBAP.withCallback transporter Context.SEARCH OnSearch.onSearchAPI context callbackUrl $ do
      pure $ ACL.mkOnSearchMessage dSearchRes
