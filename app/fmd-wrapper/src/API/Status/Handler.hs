module API.Status.Handler where

import API.Common
import API.Order
import qualified API.Status.Types as Status
import App.Types
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Domain.Delivery as DDelivery (Delivery (..))
import qualified Domain.Organization as DOrg
import EulerHS.Prelude hiding (id, state)
import qualified ExternalAPI.Dunzo.Types as Dz
import qualified Storage.Queries.Delivery as QDelivery
import qualified Types.Beckn.API.OnStatus as OnStatus
import Types.Beckn.Context
import Types.Error
import Types.Wrapper
import Utils.Callback
import Utils.Common

handler :: SignatureAuthResult -> BecknReq Status.OrderId -> FlowHandler AckResponse
handler (SignatureAuthResult _ subscriber) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    bapOrg <- findOrg subscriber
    validateContext STATUS $ req.context
    validateBapUrl subscriber $ req.context
    status bapOrg req

status ::
  DOrg.Organization ->
  BecknReq Status.OrderId ->
  Flow AckResponse
status org req = do
  conf@DunzoConfig {..} <- asks (.dzConfig)
  let orderId = req.context.transaction_id
  dzBACreds <- getCreds org.dunzoCredsId
  delivery <- QDelivery.findAggregatesById (Id orderId) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  withCallback STATUS OnStatus.onStatusAPI req.context req.context.bap_uri do
    taskStatus <- getStatus dzBACreds conf (Dz.TaskId delivery.deliveryServiceOrderId)
    updatedDelivery <- updateDelivery delivery taskStatus
    let onStatusOrder = mkOrder delivery
    Esq.runTransaction $ do
      QDelivery.update updatedDelivery
    pure $ OnStatus.OrderObject onStatusOrder

updateDelivery :: DDelivery.Delivery -> Dz.TaskStatus -> Flow DDelivery.Delivery
updateDelivery delivery taskStatus = do
  now <- getCurrentTime
  pure
    delivery
      { DDelivery.status = taskStatus.state,
        DDelivery.deliveryPrice = taskStatus.estimated_price,
        DDelivery.updatedAt = now
      }
