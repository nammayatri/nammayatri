module API.Status.Handler where

import API.Common
import qualified API.Status.Types as Status
import App.Types
import Beckn.Types.Core.ReqTypes
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import EulerHS.Prelude hiding (id, state)
import qualified ExternalAPI.Dunzo.Types as Dz
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Types.Beckn.API.OnConfirm as OnConfirm
import qualified Types.Beckn.API.OnStatus as OnStatus
import Types.Beckn.Context
import Types.Error
import qualified Types.Storage.Organization as SOrg
import qualified Types.Storage.SearchRequest as SSearchRequest
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
  SOrg.Organization ->
  BecknReq Status.OrderId ->
  Flow AckResponse
status org req = do
  conf@DunzoConfig {..} <- asks (.dzConfig)
  let orderId = req.context.transaction_id
  searchRequest <- QSearchRequest.findById (Id orderId) >>= fromMaybeErr "ORDER_NOT_FOUND" (Just CORE003)
  taskInfo :: Dz.TaskStatus <- searchRequest.udf2 >>= decodeFromText & fromMaybeM (InternalError "Decoding TaskStatus error.")
  let taskId = taskInfo.task_id.getTaskId
  order <-
    searchRequest.udf1 >>= decodeFromText
      & fromMaybeM (InternalError "Decode error.")
  dzBACreds <- getDzBAPCreds org
  withCallback STATUS OnStatus.onStatusAPI req.context req.context.bap_uri do
    taskStatus <- getStatus dzBACreds conf (Dz.TaskId taskId)
    now <- getCurrentTime
    let updatedOrder = updateOrder order now taskStatus
    let onStatusOrder = mkOnStatusOrder order
    let onStatusMessage = OnStatus.OrderObject onStatusOrder
    updateSearchRequest (searchRequest.id) updatedOrder taskStatus searchRequest
    return onStatusMessage
  where
    updateSearchRequest searchRequestId updatedOrder taskStatus searchRequest = do
      let updatedSearchRequest =
            searchRequest
              { SSearchRequest.udf1 = Just $ encodeToText updatedOrder,
                SSearchRequest.udf2 = Just $ encodeToText taskStatus
              }
      QSearchRequest.update searchRequestId updatedSearchRequest

mkOnStatusOrder :: OnConfirm.Order -> OnStatus.Order
mkOnStatusOrder OnConfirm.Order {..} = OnStatus.Order {..}

--TODO Which of these Order fields can really change?
updateOrder :: OnConfirm.Order -> UTCTime -> Dz.TaskStatus -> OnConfirm.Order
updateOrder order@OnConfirm.Order {..} now taskStatus = do
  let uFulfillment = order.fulfillment & #id .~ taskStatus.task_id.getTaskId
  OnConfirm.Order
    { state = mapTaskStateToOrderState (taskStatus.state),
      payment = mkPayment taskStatus.estimated_price,
      updated_at = now,
      fulfillment = uFulfillment,
      ..
    }
