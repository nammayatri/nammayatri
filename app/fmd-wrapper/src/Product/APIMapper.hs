module Product.APIMapper where

import App.Types
import Beckn.Product.Validation.Context
import Beckn.Types.Storage.Organization (Organization)
import EulerHS.Prelude
import qualified Product.Dunzo.Flow as DZ
import Types.Beckn.API.Cancel (CancellationInfo)
import Types.Beckn.API.Init (InitOrder)
import Types.Beckn.API.Search (SearchIntent)
import Types.Beckn.API.Select (SelectedObject)
import Types.Beckn.API.Status (OrderId)
import Types.Beckn.API.Track (TrackInfo)
import qualified Types.Beckn.API.Types as API
import Types.Beckn.API.Update (UpdateInfo)
import Types.Beckn.Context
import qualified Types.Beckn.Domain as Domain
import Types.Error
import Utils.Common

-- TODO: add switching logic to figure out the client instance
search :: Organization -> API.BecknReq SearchIntent -> FlowHandler AckResponse
search org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTagMig req $ do
    validateContext SEARCH $ req.context
    DZ.search org req

select :: Organization -> API.BecknReq SelectedObject -> FlowHandler AckResponse
select org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTagMig req $ do
    validateContext SELECT $ req.context
    validateBapUrl org $ req.context
    DZ.select org req

init :: Organization -> API.BecknReq InitOrder -> FlowHandler AckResponse
init org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTagMig req $ do
    validateContext INIT $ req.context
    validateBapUrl org $ req.context
    DZ.init org req

confirm :: Organization -> API.BecknReq API.OrderObject -> FlowHandler AckResponse
confirm org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTagMig req $ do
    validateContext CONFIRM $ req.context
    validateBapUrl org $ req.context
    DZ.confirm org req

track :: Organization -> API.BecknReq TrackInfo -> FlowHandler AckResponse
track org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTagMig req $ do
    validateContext TRACK $ req.context
    validateBapUrl org $ req.context
    DZ.track org req

status :: Organization -> API.BecknReq OrderId -> FlowHandler AckResponse
status org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTagMig req $ do
    validateContext STATUS $ req.context
    validateBapUrl org $ req.context
    DZ.status org req

cancel :: Organization -> API.BecknReq CancellationInfo -> FlowHandler AckResponse
cancel org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTagMig req $ do
    validateContext CANCEL $ req.context
    validateBapUrl org $ req.context
    DZ.cancel org req

update :: Organization -> API.BecknReq UpdateInfo -> FlowHandler AckResponse
update org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTagMig req $ do
    validateContext UPDATE $ req.context
    validateBapUrl org $ req.context
    DZ.update org req

validateContext :: Action -> Context -> Flow ()
validateContext action context = do
  validateDomainMig Domain.FINAL_MILE_DELIVERY context
  validateContextCommonsMig action context

validateBapUrl :: Organization -> Context -> Flow ()
validateBapUrl org context =
  unless (org.callbackUrl == Just context.bap_uri) $
    throwError (InvalidRequest "Invalid bap URL.")
