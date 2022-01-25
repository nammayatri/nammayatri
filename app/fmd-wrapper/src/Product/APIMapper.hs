module Product.APIMapper where

import App.Types
import Beckn.Product.Validation.Context hiding (validateContext)
import Beckn.Types.Core.Migration.Order
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Id
import Beckn.Types.Registry.Subscriber (Subscriber)
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import EulerHS.Prelude
import qualified Product.Dunzo.Flow as DZ
import qualified Storage.Queries.Organization as Org
import Types.Beckn.API.Cancel (CancellationInfo)
import Types.Beckn.API.Search (SearchIntent)
import Types.Beckn.API.Status (OrderId)
import Types.Beckn.API.Track (TrackInfo)
import Types.Beckn.Context
import qualified Types.Beckn.Domain as Domain
import Types.Error
import qualified Types.Storage.Organization as Org
import Utils.Common

findOrg :: DBFlow m r => Subscriber -> m Org.Organization
findOrg subscriber =
  Org.findOrgByShortId (ShortId subscriber.subscriber_id)
    >>= fromMaybeM OrgDoesNotExist

search :: SignatureAuthResult -> SignatureAuthResult -> BecknReq SearchIntent -> FlowHandler AckResponse
search (SignatureAuthResult _ subscriber) (SignatureAuthResult _ gateway) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    validateContext SEARCH $ req.context
    validateBapUrl subscriber $ req.context
    bapOrg <- findOrg subscriber
    DZ.search bapOrg gateway.subscriber_url req

confirm :: SignatureAuthResult -> BecknReq OrderObject -> FlowHandler AckResponse
confirm (SignatureAuthResult _ subscriber) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    bapOrg <- findOrg subscriber
    validateContext CONFIRM $ req.context
    validateBapUrl subscriber $ req.context
    DZ.confirm bapOrg req

track :: SignatureAuthResult -> BecknReq TrackInfo -> FlowHandler AckResponse
track (SignatureAuthResult _ subscriber) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    bapOrg <- findOrg subscriber
    validateContext TRACK $ req.context
    validateBapUrl subscriber $ req.context
    DZ.track bapOrg req

status :: SignatureAuthResult -> BecknReq OrderId -> FlowHandler AckResponse
status (SignatureAuthResult _ subscriber) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    bapOrg <- findOrg subscriber
    validateContext STATUS $ req.context
    validateBapUrl subscriber $ req.context
    DZ.status bapOrg req

cancel :: SignatureAuthResult -> BecknReq CancellationInfo -> FlowHandler AckResponse
cancel (SignatureAuthResult _ subscriber) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    bapOrg <- findOrg subscriber
    validateContext CANCEL $ req.context
    validateBapUrl subscriber $ req.context
    DZ.cancel bapOrg req

validateContext :: HasFlowEnv m r '["coreVersion" ::: Text] => Action -> Context -> m ()
validateContext action context = do
  validateDomainMig Domain.FINAL_MILE_DELIVERY context
  validateContextCommonsMig action context

validateBapUrl :: MonadFlow m => Subscriber -> Context -> m ()
validateBapUrl subscriber context =
  unless (subscriber.subscriber_url == context.bap_uri) $
    throwError (InvalidRequest "Invalid bap URL.")
