{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnStatus
  ( buildOnStatusMessage,
    mkOnStatusMessageV2,
  )
where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.ACL.Common.Order as Common
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.OnDemand.Utils.OnUpdate as UtilsOU
import qualified Beckn.Types.Core.Taxi.Common.Tags as Tags
import qualified Beckn.Types.Core.Taxi.OnStatus as OnStatus
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.BookingCancelledOrder as BookingCancelledOS
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.BookingReallocationOrder as BookingReallocationOS
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.NewBookingOrder as NewBookingOS
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.RideAssignedOrder as RideAssignedOS
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.RideCompletedOrder as RideCompletedOS
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.RideStartedOrder as RideStartedOS
import qualified BecknV2.OnDemand.Types as Spec
import qualified Domain.Action.Beckn.Status as DStatus
import Kernel.Prelude
import Kernel.Types.Common
import qualified SharedLogic.SyncRide as SyncRide

buildOnStatusMessage ::
  (EsqDBFlow m r, EncFlow m r) =>
  DStatus.OnStatusBuildReq ->
  m OnStatus.OnStatusMessage
buildOnStatusMessage (DStatus.NewBookingBuildReq {bookingId}) = do
  return $
    OnStatus.OnStatusMessage
      { order =
          OnStatus.NewBooking $
            NewBookingOS.NewBookingOrder
              { id = bookingId.getId,
                state = NewBookingOS.orderState,
                ..
              }
      }
buildOnStatusMessage (DStatus.RideAssignedBuildReq {newRideInfo}) = do
  let SyncRide.NewRideInfo {driver, image, vehicle, ride, booking} = newRideInfo
  let arrivalTimeTagGroup = Common.mkArrivalTimeTagGroup ride.driverArrivalTime
  fulfillment <- Common.mkFulfillment (Just driver) ride booking (Just vehicle) image (Just $ Tags.TG arrivalTimeTagGroup) Nothing False False
  return $
    OnStatus.OnStatusMessage
      { order =
          OnStatus.RideAssigned $
            RideAssignedOS.RideAssignedOrder
              { id = booking.id.getId,
                state = RideAssignedOS.orderState,
                ..
              }
      }
buildOnStatusMessage (DStatus.RideStartedBuildReq {newRideInfo}) = do
  let SyncRide.NewRideInfo {driver, image, vehicle, ride, booking} = newRideInfo
  let arrivalTimeTagGroup = Common.mkArrivalTimeTagGroup ride.driverArrivalTime
  fulfillment <- Common.mkFulfillment (Just driver) ride booking (Just vehicle) image (Just $ Tags.TG arrivalTimeTagGroup) Nothing False False
  return $
    OnStatus.OnStatusMessage
      { order =
          OnStatus.RideStarted $
            RideStartedOS.RideStartedOrder
              { id = booking.id.getId,
                state = RideStartedOS.orderState,
                ..
              }
      }
buildOnStatusMessage (DStatus.RideCompletedBuildReq {newRideInfo, rideCompletedInfo}) = do
  let SyncRide.NewRideInfo {driver, image, vehicle, ride, booking} = newRideInfo
  let SyncRide.RideCompletedInfo {fareParams, paymentMethodInfo, paymentUrl} = rideCompletedInfo
  let arrivalTimeTagGroup = Common.mkArrivalTimeTagGroup ride.driverArrivalTime
  distanceTagGroup <- Common.buildDistanceTagGroup ride
  fulfillment <- Common.mkFulfillment (Just driver) ride booking (Just vehicle) image (Just $ Tags.TG (distanceTagGroup <> arrivalTimeTagGroup)) Nothing False False
  quote <- Common.buildRideCompletedQuote ride fareParams
  return $
    OnStatus.OnStatusMessage
      { order =
          OnStatus.RideCompleted
            RideCompletedOS.RideCompletedOrder
              { id = booking.id.getId,
                state = RideCompletedOS.orderState,
                quote,
                payment = Just $ Common.mkRideCompletedPayment paymentMethodInfo paymentUrl,
                fulfillment = fulfillment
              }
      }
buildOnStatusMessage DStatus.BookingCancelledBuildReq {bookingCancelledInfo, mbNewRideInfo} = do
  let SyncRide.BookingCancelledInfo {booking, cancellationSource} = bookingCancelledInfo
  fulfillment <- forM mbNewRideInfo $ \newRideInfo -> do
    let SyncRide.NewRideInfo {driver, image, vehicle, ride} = newRideInfo
    let arrivalTimeTagGroup = Common.mkArrivalTimeTagGroup ride.driverArrivalTime
    Common.mkFulfillment (Just driver) ride booking (Just vehicle) image (Just $ Tags.TG arrivalTimeTagGroup) Nothing False False
  pure
    OnStatus.OnStatusMessage
      { order =
          OnStatus.BookingCancelled $
            BookingCancelledOS.BookingCancelledOrder
              { id = booking.id.getId,
                state = BookingCancelledOS.orderState,
                cancellation_reason = Common.castCancellationSource cancellationSource,
                fulfillment
              }
      }
buildOnStatusMessage DStatus.BookingReallocationBuildReq {bookingReallocationInfo, newRideInfo} = do
  let SyncRide.BookingCancelledInfo {booking, cancellationSource} = bookingReallocationInfo
  let SyncRide.NewRideInfo {driver, image, vehicle, ride} = newRideInfo
  let arrivalTimeTagGroup = Common.mkArrivalTimeTagGroup ride.driverArrivalTime
  fulfillment <- Common.mkFulfillment (Just driver) ride booking (Just vehicle) image (Just $ Tags.TG arrivalTimeTagGroup) Nothing False False
  pure
    OnStatus.OnStatusMessage
      { order =
          OnStatus.BookingReallocation $
            BookingReallocationOS.BookingReallocationOrder
              { id = booking.id.getId,
                state = BookingReallocationOS.orderState, -----
                reallocation_reason = Common.castCancellationSource cancellationSource,
                fulfillment
              }
      }

mkOnStatusMessageV2 ::
  (EsqDBFlow m r, EncFlow m r) =>
  DStatus.OnStatusBuildReq ->
  m (Maybe Spec.ConfirmReqMessage)
mkOnStatusMessageV2 res = do
  order <- tfOrder res
  pure . Just $
    Spec.ConfirmReqMessage
      { confirmReqMessageOrder = order
      }

tfOrder :: (MonadFlow m, EncFlow m r) => DStatus.OnStatusBuildReq -> m Spec.Order
tfOrder (DStatus.NewBookingBuildReq {bookingId}) =
  pure
    Spec.Order
      { orderId = Just bookingId.getId,
        orderStatus = Just $ show NewBookingOS.orderState,
        orderFulfillments = Nothing,
        orderBilling = Nothing,
        orderCancellation = Nothing,
        orderCancellationTerms = Nothing,
        orderItems = Nothing,
        orderPayments = Nothing,
        orderProvider = Nothing,
        orderQuote = Nothing
      }
tfOrder (DStatus.RideAssignedBuildReq {newRideInfo}) = do
  let SyncRide.NewRideInfo {driver, image, vehicle, ride, booking} = newRideInfo
  let arrivalTimeTagGroup = UtilsOU.mkDriverArrivedInfoTags ride.driverArrivalTime
  fulfillment <- Utils.mkFulfillmentV2 (Just driver) ride booking (Just vehicle) image arrivalTimeTagGroup Nothing False False Nothing
  pure
    Spec.Order
      { orderId = Just $ booking.id.getId,
        orderStatus = Just $ show RideAssignedOS.orderState,
        orderFulfillments = Just [fulfillment],
        orderBilling = Nothing,
        orderCancellation = Nothing,
        orderCancellationTerms = Nothing,
        orderItems = Nothing,
        orderPayments = Nothing,
        orderProvider = Nothing,
        orderQuote = Nothing
      }
tfOrder (DStatus.RideStartedBuildReq {newRideInfo}) = do
  let SyncRide.NewRideInfo {driver, image, vehicle, ride, booking} = newRideInfo
  let arrivalTimeTagGroup = UtilsOU.mkDriverArrivedInfoTags ride.driverArrivalTime
  fulfillment <- Utils.mkFulfillmentV2 (Just driver) ride booking (Just vehicle) image arrivalTimeTagGroup Nothing False False Nothing
  pure
    Spec.Order
      { orderId = Just $ booking.id.getId,
        orderStatus = Just $ show RideStartedOS.orderState,
        orderFulfillments = Just [fulfillment],
        orderBilling = Nothing,
        orderCancellation = Nothing,
        orderCancellationTerms = Nothing,
        orderItems = Nothing,
        orderPayments = Nothing,
        orderProvider = Nothing,
        orderQuote = Nothing
      }
tfOrder (DStatus.RideCompletedBuildReq {newRideInfo, rideCompletedInfo}) = do
  let SyncRide.NewRideInfo {driver, image, vehicle, ride, booking} = newRideInfo
  let SyncRide.RideCompletedInfo {fareParams, paymentMethodInfo, paymentUrl} = rideCompletedInfo
  let arrivalTimeTagGroup = UtilsOU.mkDriverArrivedInfoTags ride.driverArrivalTime
  distanceTagGroup <- UtilsOU.mkDistanceTagGroup ride
  fulfillment <- Utils.mkFulfillmentV2 (Just driver) ride booking (Just vehicle) image (arrivalTimeTagGroup <> distanceTagGroup) Nothing False False Nothing
  quote <- UtilsOU.mkRideCompletedQuote ride fareParams
  pure
    Spec.Order
      { orderId = Just $ booking.id.getId,
        orderStatus = Just $ show RideCompletedOS.orderState,
        orderFulfillments = Just [fulfillment],
        orderPayments = Just $ UtilsOU.mkRideCompletedPayment paymentMethodInfo paymentUrl,
        orderQuote = Just quote,
        orderBilling = Nothing,
        orderCancellation = Nothing,
        orderCancellationTerms = Nothing,
        orderItems = Nothing,
        orderProvider = Nothing
      }
tfOrder (DStatus.BookingCancelledBuildReq {bookingCancelledInfo, mbNewRideInfo}) = do
  let SyncRide.BookingCancelledInfo {booking, cancellationSource} = bookingCancelledInfo
  fulfillment <- forM mbNewRideInfo $ \newRideInfo -> do
    let SyncRide.NewRideInfo {driver, image, vehicle, ride} = newRideInfo
    let arrivalTimeTagGroup = UtilsOU.mkDriverArrivedInfoTags ride.driverArrivalTime
    Utils.mkFulfillmentV2 (Just driver) ride booking (Just vehicle) image arrivalTimeTagGroup Nothing False False Nothing
  pure
    Spec.Order
      { orderId = Just $ booking.id.getId,
        orderStatus = Just $ show BookingCancelledOS.orderState,
        orderFulfillments = Just $ maybeToList fulfillment,
        orderCancellation =
          Just $
            Spec.Cancellation
              { cancellationCancelledBy = Just . show $ UtilsOU.castCancellationSource cancellationSource
              },
        orderBilling = Nothing,
        orderCancellationTerms = Nothing,
        orderItems = Nothing,
        orderPayments = Nothing,
        orderProvider = Nothing,
        orderQuote = Nothing
      }
tfOrder (DStatus.BookingReallocationBuildReq {bookingReallocationInfo, newRideInfo}) = do
  let SyncRide.BookingCancelledInfo {booking, cancellationSource} = bookingReallocationInfo
  let SyncRide.NewRideInfo {driver, image, vehicle, ride} = newRideInfo
  let arrivalTimeTagGroup = UtilsOU.mkDriverArrivedInfoTags ride.driverArrivalTime
  fulfillment <- Utils.mkFulfillmentV2 (Just driver) ride booking (Just vehicle) image arrivalTimeTagGroup Nothing False False Nothing
  pure
    Spec.Order
      { orderId = Just $ booking.id.getId,
        orderStatus = Just $ show BookingReallocationOS.orderState,
        orderFulfillments = Just [fulfillment],
        orderCancellation =
          Just $
            Spec.Cancellation
              { cancellationCancelledBy = Just . show $ UtilsOU.castCancellationSource cancellationSource
              },
        orderBilling = Nothing,
        orderCancellationTerms = Nothing,
        orderItems = Nothing,
        orderPayments = Nothing,
        orderProvider = Nothing,
        orderQuote = Nothing
      }
