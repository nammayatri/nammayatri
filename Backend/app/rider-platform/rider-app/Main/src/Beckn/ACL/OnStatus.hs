{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnStatus (buildOnStatusReqV2) where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Domain.Action.Beckn.OnStatus as DOnStatus
import Control.Lens ((^?), _Just, _head)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id (Id (Id))
import Kernel.Utils.Common
import Tools.Error (GenericError (InvalidRequest))

buildOnStatusReqV2 ::
  ( HasFlowEnv m r '["_version" ::: Text],
    MonadFlow m,
    CacheFlow m r
  ) =>
  Spec.OnStatusReq ->
  Text ->
  m (Maybe DOnStatus.DOnStatusReq)
buildOnStatusReqV2 req txnId = do
  ContextV2.validateContext Context.ON_STATUS req.onStatusReqContext
  handleErrorV2 req \message -> do
    let order = message.confirmReqMessageOrder
    messageId <- Utils.getMessageId req.onStatusReqContext
    bppBookingIdText <- order.orderId & fromMaybeM (InvalidRequest "order.id is not present in on_status request.")
    let bppBookingId = Id bppBookingIdText
    orderStatus <- order.orderStatus & fromMaybeM (InvalidRequest "order.status is not present in on_status request.")
    eventType <-
      order.orderFulfillments
        ^? _Just . _head
        >>= (.fulfillmentState)
        >>= (.fulfillmentStateDescriptor)
        >>= (.descriptorCode)
        & fromMaybeM (InvalidRequest "Event type is not present in OnUpdateReq.")

    rideDetails <-
      -- TODO::Beckn, need to refactor this codes, according to spec.
      case orderStatus of
        "NEW_BOOKING" -> pure DOnStatus.NewBookingDetails
        "RIDE_BOOKING_REALLOCATION" -> parseRideBookingReallocationOrder order messageId txnId
        "ACTIVE" -> do
          case eventType of
            "RIDE_ASSIGNED" -> do
              assignedReq <- Common.parseRideAssignedEvent order messageId txnId
              return $ DOnStatus.RideAssignedDetails assignedReq
            "RIDE_ENROUTE_PICKUP" -> pure DOnStatus.RideEnroutePickupDetails
            "RIDE_ARRIVED_PICKUP" -> do
              arrivedReq <- Common.parseDriverArrivedEvent order messageId txnId
              return $ DOnStatus.DriverArrivedDetails arrivedReq
            "RIDE_STARTED" -> do
              startedReq <- Common.parseRideStartedEvent order messageId txnId
              return $ DOnStatus.RideStartedDetails startedReq
            _ -> throwError $ InvalidRequest $ "Invalid event type: " <> eventType
        "COMPLETE" -> do
          case eventType of
            "RIDE_ENDED" -> do
              completedReq <- Common.parseRideCompletedEvent order messageId txnId
              return $ DOnStatus.RideCompletedDetails completedReq
            _ -> throwError $ InvalidRequest $ "Invalid event type: " <> eventType
        "CANCELLED" -> do
          case eventType of
            "RIDE_CANCELLED" -> do
              cancelledReq <- Common.parseBookingCancelledEvent order messageId txnId
              return $ DOnStatus.BookingCancelledDetails cancelledReq
            _ -> throwError $ InvalidRequest $ "Invalid event type: " <> eventType
        _ -> throwError . InvalidRequest $ "Invalid order.status: " <> show orderStatus
    pure $
      DOnStatus.DOnStatusReq
        { bppBookingId,
          rideDetails
        }

parseRideBookingReallocationOrder :: (MonadFlow m, CacheFlow m r) => Spec.Order -> Text -> Text -> m DOnStatus.RideDetails
parseRideBookingReallocationOrder order messageId txnId = do
  bookingDetails <- Common.parseBookingDetails order messageId
  reallocationSourceText <- order.orderCancellation >>= (.cancellationCancelledBy) & fromMaybeM (InvalidRequest "order.cancellation.,cancelled_by is not present in on_status BookingReallocationEvent request.")
  let reallocationSource = Utils.castCancellationSourceV2 reallocationSourceText
  let transactionId = txnId
  pure $ DOnStatus.BookingReallocationDetails DOnStatus.BookingReallocationReq {..}

handleErrorV2 ::
  (MonadFlow m) =>
  Spec.OnStatusReq ->
  (Spec.ConfirmReqMessage -> m DOnStatus.DOnStatusReq) ->
  m (Maybe DOnStatus.DOnStatusReq)
handleErrorV2 req action =
  case req.onStatusReqError of
    Nothing -> do
      onStatusMessage <- req.onStatusReqMessage & fromMaybeM (InvalidRequest "on_status request message is not present.")
      Just <$> action onStatusMessage
    Just err -> do
      logTagError "on_status req" $ "on_status error: " <> show err
      pure Nothing
