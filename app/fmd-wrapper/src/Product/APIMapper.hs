module Product.APIMapper where

import App.Types
import Beckn.Product.Validation.Context
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
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
search :: SignatureAuthResult Organization -> API.BecknReq SearchIntent -> FlowHandler AckResponse
search (SignatureAuthResult _ bapOrg) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTagMig req $ do
    validateContext SEARCH $ req.context
    DZ.search bapOrg req

select :: SignatureAuthResult Organization -> API.BecknReq SelectedObject -> FlowHandler AckResponse
select (SignatureAuthResult _ bapOrg) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTagMig req $ do
    validateContext SELECT $ req.context
    validateBapUrl bapOrg $ req.context
    DZ.select bapOrg req

init :: SignatureAuthResult Organization -> API.BecknReq InitOrder -> FlowHandler AckResponse
init (SignatureAuthResult _ bapOrg) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTagMig req $ do
    validateContext INIT $ req.context
    validateBapUrl bapOrg $ req.context
    DZ.init bapOrg req

confirm :: SignatureAuthResult Organization -> API.BecknReq API.OrderObject -> FlowHandler AckResponse
confirm (SignatureAuthResult _ bapOrg) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTagMig req $ do
    validateContext CONFIRM $ req.context
    validateBapUrl bapOrg $ req.context
    DZ.confirm bapOrg req

track :: SignatureAuthResult Organization -> API.BecknReq TrackInfo -> FlowHandler AckResponse
track (SignatureAuthResult _ bapOrg) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTagMig req $ do
    validateContext TRACK $ req.context
    validateBapUrl bapOrg $ req.context
    DZ.track bapOrg req

status :: SignatureAuthResult Organization -> API.BecknReq OrderId -> FlowHandler AckResponse
status (SignatureAuthResult _ bapOrg) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTagMig req $ do
    validateContext STATUS $ req.context
    validateBapUrl bapOrg $ req.context
    DZ.status bapOrg req

cancel :: SignatureAuthResult Organization -> API.BecknReq CancellationInfo -> FlowHandler AckResponse
cancel (SignatureAuthResult _ bapOrg) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTagMig req $ do
    validateContext CANCEL $ req.context
    validateBapUrl bapOrg $ req.context
    DZ.cancel bapOrg req

update :: SignatureAuthResult Organization -> API.BecknReq UpdateInfo -> FlowHandler AckResponse
update (SignatureAuthResult _ bapOrg) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTagMig req $ do
    validateContext UPDATE $ req.context
    validateBapUrl bapOrg $ req.context
    DZ.update bapOrg req

validateContext :: HasFlowEnv m r '["coreVersion" ::: Text] => Action -> Context -> m ()
validateContext action context = do
  validateDomainMig Domain.FINAL_MILE_DELIVERY context
  validateContextCommonsMig action context

validateBapUrl :: MonadFlow m => Organization -> Context -> m ()
validateBapUrl bapUrl context =
  unless (bapUrl.callbackUrl == Just context.bap_uri) $
    throwError (InvalidRequest "Invalid bap URL.")
