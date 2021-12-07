module Product.APIMapper where

import App.Types
import Beckn.Product.Validation.Context hiding (validateContext)
import Beckn.Types.Id
import Beckn.Types.Registry.Subscriber (Subscriber)
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import EulerHS.Prelude
import qualified Product.Dunzo.Flow as DZ
import qualified Storage.Queries.Organization as Org
import Types.Beckn.API.Cancel (CancellationInfo)
import Types.Beckn.API.Init (InitOrderObj)
import Types.Beckn.API.Search (SearchIntent)
import Types.Beckn.API.Select (SelectedObject)
import Types.Beckn.API.Status (OrderId)
import Types.Beckn.API.Track (TrackInfo)
import qualified Types.Beckn.API.Types as API
import Types.Beckn.API.Update (UpdateInfo)
import Types.Beckn.Context
import qualified Types.Beckn.Domain as Domain
import Types.Error
import qualified Types.Storage.Organization as Org
import Utils.Common

findOrg :: DBFlow m r => Subscriber -> m Org.Organization
findOrg subscriber =
  Org.findOrgByShortId (ShortId subscriber.subscriber_id)
    >>= fromMaybeM OrgDoesNotExist

-- TODO: add switching logic to figure out the client instance
search :: SignatureAuthResult -> API.BecknReq SearchIntent -> FlowHandler AckResponse
search (SignatureAuthResult _ subscriber) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    validateContext SEARCH $ req.context
    bapOrg <- findOrg subscriber
    DZ.search bapOrg req

select :: SignatureAuthResult -> API.BecknReq SelectedObject -> FlowHandler AckResponse
select (SignatureAuthResult _ subscriber) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    validateContext SELECT $ req.context
    bapOrg <- findOrg subscriber
    validateBapUrl subscriber $ req.context
    DZ.select bapOrg req

init :: SignatureAuthResult -> API.BecknReq InitOrderObj -> FlowHandler AckResponse
init (SignatureAuthResult _ subscriber) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    bapOrg <- findOrg subscriber
    validateContext INIT $ req.context
    validateBapUrl subscriber $ req.context
    DZ.init bapOrg req

confirm :: SignatureAuthResult -> API.BecknReq API.OrderObject -> FlowHandler AckResponse
confirm (SignatureAuthResult _ subscriber) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    bapOrg <- findOrg subscriber
    validateContext CONFIRM $ req.context
    validateBapUrl subscriber $ req.context
    DZ.confirm bapOrg req

track :: SignatureAuthResult -> API.BecknReq TrackInfo -> FlowHandler AckResponse
track (SignatureAuthResult _ subscriber) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    bapOrg <- findOrg subscriber
    validateContext TRACK $ req.context
    validateBapUrl subscriber $ req.context
    DZ.track bapOrg req

status :: SignatureAuthResult -> API.BecknReq OrderId -> FlowHandler AckResponse
status (SignatureAuthResult _ subscriber) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    bapOrg <- findOrg subscriber
    validateContext STATUS $ req.context
    validateBapUrl subscriber $ req.context
    DZ.status bapOrg req

cancel :: SignatureAuthResult -> API.BecknReq CancellationInfo -> FlowHandler AckResponse
cancel (SignatureAuthResult _ subscriber) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    bapOrg <- findOrg subscriber
    validateContext CANCEL $ req.context
    validateBapUrl subscriber $ req.context
    DZ.cancel bapOrg req

update :: SignatureAuthResult -> API.BecknReq UpdateInfo -> FlowHandler AckResponse
update (SignatureAuthResult _ subscriber) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    bapOrg <- findOrg subscriber
    validateContext UPDATE $ req.context
    validateBapUrl subscriber $ req.context
    DZ.update bapOrg req

validateContext :: HasFlowEnv m r '["coreVersion" ::: Text] => Action -> Context -> m ()
validateContext action context = do
  validateDomainMig Domain.FINAL_MILE_DELIVERY context
  validateContextCommonsMig action context

validateBapUrl :: MonadFlow m => Subscriber -> Context -> m ()
validateBapUrl _subscriber _context = pure ()

-- Depends on registry fix
-- unless (subscriber.callback_url == Just context.bap_uri) $
--   throwError (InvalidRequest "Invalid bap URL.")
