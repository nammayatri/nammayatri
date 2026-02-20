{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Update (buildUpdateReq) where

import qualified Beckn.OnDemand.Utils.Common as Common
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import Control.Lens ((^?), _Just, _head)
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import Data.Text (toLower)
import qualified Domain.Action.Beckn.Update as DUpdate
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import EulerHS.Prelude hiding (state, (^?))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Common
import Tools.Error (GenericError (InvalidRequest))

buildUpdateReq ::
  (HasFlowEnv m r '["_version" ::: Text]) =>
  Id DM.Merchant ->
  Subscriber.Subscriber ->
  Spec.UpdateReq ->
  m DUpdate.DUpdateReq
buildUpdateReq merchantId subscriber req = do
  ContextV2.validateContext Context.UPDATE req.updateReqContext
  unless (Just subscriber.subscriber_id == req.updateReqContext.contextBapId) $
    throwError (InvalidRequest $ "Invalid bap_id " <> "bap_id: " <> show req.updateReqContext.contextBapId <> "subscriber_id: " <> show subscriber.subscriber_id)
  parseEvent merchantId req.updateReqMessage req.updateReqContext

parseEvent :: (MonadFlow m) => Id DM.Merchant -> Spec.UpdateReqMessage -> Spec.Context -> m DUpdate.DUpdateReq
parseEvent merchantId reqMsg context = do
  bookingId <- fmap Id reqMsg.updateReqMessageOrder.orderId & fromMaybeM (InvalidRequest "orderId not found")
  fulfillment <- reqMsg.updateReqMessageOrder.orderFulfillments ^? _Just . _head & fromMaybeM (InvalidRequest "Fulfillment not found")
  eventType <-
    fulfillment.fulfillmentState
      >>= (.fulfillmentStateDescriptor)
      >>= (.descriptorCode)
      & fromMaybeM (InvalidRequest "Event type is not present in UpdateReq.")

  case eventType of
    "PAYMENT_COMPLETED" -> do
      rideId <- fmap Id fulfillment.fulfillmentId & fromMaybeM (InvalidRequest "Fulfillment id not found")
      payment <- reqMsg.updateReqMessageOrder.orderPayments ^? _Just . _head & fromMaybeM (InvalidRequest "Payment not present")
      paymentMethodInfo <- mkPaymentMethodInfo payment
      pure $
        DUpdate.UPaymentCompletedReq $
          DUpdate.PaymentCompletedReq
            { bookingId,
              rideId,
              paymentStatus = castPaymentStatus payment.paymentStatus,
              paymentMethodInfo
            }
    "EDIT_LOCATION" -> do
      rideId <- fmap Id fulfillment.fulfillmentId & fromMaybeM (InvalidRequest "Fulfillment id not found")
      parseEditLocationEvent bookingId fulfillment rideId
    "ADD_STOP" -> parseAddStopEvent bookingId fulfillment
    "EDIT_STOP" -> parseEditStopEvent bookingId fulfillment
    _ -> throwError (InvalidRequest "Invalid event type")
  where
    parseAddStopEvent bookingId fulfillment = do
      fulfillmentStops <- fulfillment.fulfillmentStops & fromMaybeM (InvalidRequest "Fulfillment stops not found")
      stops' <- mapM (Utils.buildLocation' merchantId) fulfillmentStops
      pure $
        DUpdate.UAddStopReq $
          DUpdate.AddStopReq
            { bookingId,
              stops'
            }

    parseEditStopEvent bookingId fulfillment = do
      fulfillmentStops <- fulfillment.fulfillmentStops & fromMaybeM (InvalidRequest "Fulfillment stops not found")
      stops' <- mapM (Utils.buildLocation' merchantId) fulfillmentStops
      pure $
        DUpdate.UEditStopReq $
          DUpdate.EditStopReq
            { bookingId,
              stops'
            }

    parseEditLocationEvent bookingId fulfillment rideId = do
      fulfillmentStops <- fulfillment.fulfillmentStops & fromMaybeM (InvalidRequest "Fulfillment stops not found")
      let originStop = Utils.getStartLocation fulfillmentStops
      origin' <- traverse (Utils.buildLocation' merchantId) originStop
      let destinationStop = Utils.getDropLocation fulfillmentStops
      destination' <- traverse (Utils.buildLocation' merchantId) destinationStop
      orderStatus <- reqMsg.updateReqMessageOrder.orderStatus & fromMaybeM (InvalidRequest "orderStatus not found")
      messageId <- Utils.getMessageId context
      status <- castOrderStatus orderStatus
      transactionId <- Utils.getTransactionId context
      pure $
        DUpdate.UEditLocationReq $
          DUpdate.EditLocationReq
            { bookingId,
              rideId,
              origin',
              destination',
              status,
              bapBookingUpdateRequestId = messageId,
              transactionId
            }

castOrderStatus :: (MonadFlow m) => Text -> m Enums.OrderStatus
castOrderStatus status =
  case toLower status of
    "soft_update" -> pure Enums.SOFT_UPDATE
    "confirm_update" -> pure Enums.CONFIRM_UPDATE
    _ -> throwError (InvalidRequest "Invalid order status in Edit Location Event")

mkPaymentMethodInfo :: (MonadFlow m) => Spec.Payment -> m DMPM.PaymentMethodInfo
mkPaymentMethodInfo Spec.Payment {..} = do
  collectedBy <- Common.castPaymentCollector (fromMaybe "" paymentCollectedBy)
  paymentType' <- Common.castPaymentType (fromMaybe "" paymentType)
  return $
    DMPM.PaymentMethodInfo
      { collectedBy = collectedBy,
        paymentType = paymentType',
        paymentInstrument = DMPM.Cash
      }

castPaymentStatus :: Maybe Text -> DUpdate.PaymentStatus
castPaymentStatus (Just "PAID") = DUpdate.PAID
castPaymentStatus (Just "NOT_PAID") = DUpdate.NOT_PAID
castPaymentStatus _ = DUpdate.NOT_PAID
