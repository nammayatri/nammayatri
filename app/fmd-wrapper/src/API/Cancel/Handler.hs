module API.Cancel.Handler where

import qualified API.Cancel.Types as Cancel
import API.Common
import API.Order
import App.Types
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Domain.Delivery as DDelivery
import qualified Domain.DunzoCreds as DDunzoCreds
import qualified Domain.Organization as DOrg
import EulerHS.Prelude hiding (id, state)
import qualified ExternalAPI.Dunzo.Flow as DzAPI
import qualified ExternalAPI.Dunzo.Types as Dz
import qualified Storage.Queries.Delivery as QDelivery
import qualified Types.Beckn.API.OnCancel as OnCancel
import Types.Beckn.Context
import Types.Error
import Types.Wrapper
import Utils.Callback
import Utils.Common

handler :: SignatureAuthResult -> BecknReq Cancel.CancellationInfo -> FlowHandler AckResponse
handler (SignatureAuthResult _ subscriber) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    bapOrg <- findOrg subscriber
    validateContext CANCEL $ req.context
    validateBapUrl subscriber $ req.context
    cancel bapOrg req

cancel ::
  DOrg.Organization ->
  BecknReq Cancel.CancellationInfo ->
  Flow AckResponse
cancel org req = do
  conf@DunzoConfig {..} <- asks (.dzConfig)
  let orderId = req.context.transaction_id
  delivery <- QDelivery.findAggregatesById (Id orderId) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  dzBACreds <- getCreds org.dunzoCredsId
  withCallback CANCEL OnCancel.onCancelAPI req.context req.context.bap_uri do
    callCancelAPI dzBACreds conf (Dz.TaskId delivery.deliveryServiceOrderId)
    updatedDelivery <- updateDelivery delivery
    let onCancelOrder = mkOrder updatedDelivery
    Esq.runTransaction $ do
      QDelivery.update updatedDelivery
    return $ OnCancel.OrderObject onCancelOrder
  where
    callCancelAPI dzBACreds@DDunzoCreds.DunzoCreds {..} conf@DunzoConfig {..} taskId = do
      token <- fetchToken dzBACreds conf
      -- TODO get cancellation reason
      DzAPI.cancelTask clientId token dzUrl dzTestMode taskId ""

updateDelivery :: DDelivery.Delivery -> Flow DDelivery.Delivery
updateDelivery DDelivery.Delivery {..} = do
  now <- getCurrentTime
  pure
    DDelivery.Delivery
      { status = Dz.CANCELLED,
        updatedAt = now,
        ..
      }
