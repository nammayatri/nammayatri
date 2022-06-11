module Product.BecknProvider.Search (search) where

import Beckn.Prelude
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Taxi.API.Search as Search
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.Search as ACL
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Organization as Org
import Environment
import qualified SharedLogic.Transporter as Shared
import Utils.Common

search ::
  Id Org.Organization ->
  SignatureAuthResult ->
  SignatureAuthResult ->
  Search.SearchReq ->
  FlowHandler AckResponse
search transporterId (SignatureAuthResult _ subscriber) (SignatureAuthResult _ gateway) req =
  withFlowHandlerAPI . withTransactionIdLogTag req $ do
    dSearchReq <- ACL.buildSearchReq subscriber gateway req
    transporter <- Shared.findTransporter transporterId
    DSearch.handler transporter dSearchReq
    pure Ack
