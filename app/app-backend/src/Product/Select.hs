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
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Quote as DQuote
import Domain.Types.SelectedQuote (mkSelQuoteAPIEntity)
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Storage.Queries.SelectedQuote as QSQuote
import Types.API.Select
import Utils.Common

select :: Id DPerson.Person -> Id DQuote.Quote -> FlowHandler APISuccess
select personId quoteId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  dSelectReq <- DSelect.select personId quoteId
  becknReq <- ACL.buildSelectReq dSelectReq
  void $ ExternalAPI.select dSelectReq.providerUrl becknReq
  pure Success

onSelect ::
  SignatureAuthResult ->
  OnSelect.OnSelectReq ->
  FlowHandler AckResponse
onSelect _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnSelectReq <- ACL.buildOnSelectReq req
  whenJust mbDOnSelectReq DOnSelect.onSelect
  pure Ack

selectList :: Id DPerson.Person -> Id DQuote.Quote -> FlowHandler SelectListRes
selectList personId quoteId = withFlowHandlerAPI . withPersonIdLogTag personId $ do
  selectedQuotes <- QSQuote.findByQuoteId quoteId
  pure $ SelectListRes $ map mkSelQuoteAPIEntity selectedQuotes
