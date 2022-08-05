module Product.Select where

import App.Types
import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess (Success))
import qualified Beckn.Types.Core.Taxi.API.OnSelect as OnSelect
import Beckn.Types.Id
import Beckn.Utils.Logging
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.OnSelect as ACL
import qualified Core.ACL.Select as ACL
import qualified Domain.Action.Beckn.OnSelect as DOnSelect
import qualified Domain.Action.UI.Select as DSelect
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Quote as DQuote
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.Quote as QQuote
import Types.API.Select
import Utils.Common

select :: Id DPerson.Person -> Id DEstimate.Estimate -> FlowHandler APISuccess
select personId estimateId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  dSelectReq <- DSelect.select personId estimateId
  becknReq <- ACL.buildSelectReq dSelectReq
  void $ ExternalAPI.select dSelectReq.providerUrl becknReq
  pure Success

onSelect ::
  SignatureAuthResult ->
  OnSelect.OnSelectReq ->
  FlowHandler AckResponse
onSelect (SignatureAuthResult _ _ registryUrl) req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnSelectReq <- ACL.buildOnSelectReq req
  whenJust mbDOnSelectReq (DOnSelect.onSelect registryUrl)
  pure Ack

selectList :: Id DPerson.Person -> Id DEstimate.Estimate -> FlowHandler SelectListRes
selectList personId estimateId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  selectedQuotes <- QQuote.findAllByEstimateId estimateId
  pure $ SelectListRes $ map DQuote.makeQuoteAPIEntity selectedQuotes
