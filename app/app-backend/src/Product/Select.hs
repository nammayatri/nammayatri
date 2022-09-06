module Product.Select where

import App.Types
import Beckn.Prelude
import Beckn.Types.APISuccess (APISuccess (Success))
import Beckn.Types.Id
import Beckn.Utils.Logging
import qualified Core.ACL.Select as ACL
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

selectList :: Id DPerson.Person -> Id DEstimate.Estimate -> FlowHandler SelectListRes
selectList personId estimateId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  selectedQuotes <- QQuote.findAllByEstimateId estimateId
  pure $ SelectListRes $ map DQuote.makeQuoteAPIEntity selectedQuotes
