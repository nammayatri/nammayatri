module Product.APIMapper where

import App.Types
import Beckn.Product.Validation.Context
import qualified Beckn.Types.Core.Migration.Context as Mig
import qualified Beckn.Types.Core.Migration.Domain as Mig
import Beckn.Types.Storage.Organization (Organization)
import EulerHS.Prelude
import qualified Product.Dunzo.Flow as DZ
import Types.Beckn.API.Cancel (CancelReq (..), CancelRes)
import Types.Beckn.API.Confirm (ConfirmReq (..), ConfirmRes)
import Types.Beckn.API.Init (InitReq (..), InitRes)
import Types.Beckn.API.Search (SearchIntent)
import Types.Beckn.API.Select (SelectReq (..), SelectRes)
import Types.Beckn.API.Status (StatusReq (..), StatusRes)
import Types.Beckn.API.Track (TrackReq (..), TrackRes)
import qualified Types.Beckn.API.Types as API
import Types.Beckn.API.Update (UpdateReq (..), UpdateRes)
import Types.Beckn.Context
import Types.Beckn.Domain
import Types.Error
import Utils.Common

-- TODO: add switching logic to figure out the client instance
search :: Organization -> API.BecknReq SearchIntent -> FlowHandler AckResponse
search org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTagMig req $ do
    validateContextMig "search" $ req.context
    DZ.search org req

select :: Organization -> SelectReq -> FlowHandler SelectRes
select org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    validateContext "select" $ req.context
    validateBapUrl org $ req.context
    DZ.select org req

init :: Organization -> InitReq -> FlowHandler InitRes
init org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    validateContext "init" $ req.context
    validateBapUrl org $ req.context
    DZ.init org req

confirm :: Organization -> ConfirmReq -> FlowHandler ConfirmRes
confirm org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    validateContext "confirm" $ req.context
    validateBapUrl org $ req.context
    DZ.confirm org req

track :: Organization -> TrackReq -> FlowHandler TrackRes
track org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    validateContext "track" $ req.context
    validateBapUrl org $ req.context
    DZ.track org req

status :: Organization -> StatusReq -> FlowHandler StatusRes
status org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    validateContext "status" $ req.context
    validateBapUrl org $ req.context
    DZ.status org req

cancel :: Organization -> CancelReq -> FlowHandler CancelRes
cancel org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    validateContext "cancel" $ req.context
    validateBapUrl org $ req.context
    DZ.cancel org req

update :: Organization -> UpdateReq -> FlowHandler UpdateRes
update org req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    validateContext "update" $ req.context
    validateBapUrl org $ req.context
    DZ.update org req

validateContext :: Text -> Context -> Flow ()
validateContext action context = do
  validateDomain FINAL_MILE_DELIVERY context
  validateContextCommons action context

validateContextMig :: Text -> Mig.Context -> Flow ()
validateContextMig action context = do
  validateDomainMig (Mig.Domain "FINAL-MILE-DELIVERY") context
  validateContextCommonsMig action context

validateBapUrl :: Organization -> Context -> Flow ()
validateBapUrl org context = do
  let satisfied = case context.bap_uri of
        Nothing -> False
        Just bapUrl -> org.callbackUrl == Just bapUrl
  unless satisfied $
    throwError (InvalidRequest "Invalid bap URL.")
