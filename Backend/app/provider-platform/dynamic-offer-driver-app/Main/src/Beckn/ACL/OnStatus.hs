{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnStatus (buildOnStatusMessage) where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.Types.Core.Taxi.OnStatus as OnStatus
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.BookingCancelledOrder as BookingCancelledOS
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.NewBookingOrder as NewBookingOS
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.RideAssignedOrder as RideAssignedOS
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.RideCompletedOrder as RideCompletedOS
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.RideStartedOrder as RideStartedOS
import qualified Domain.Action.Beckn.Status as DStatus
import qualified Domain.Types.FareParameters as DFParams
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Common
import SharedLogic.FareCalculator (mkBreakupList)
import qualified SharedLogic.SyncRide as SyncRide
import Tools.Error

buildOnStatusMessage ::
  EncFlow m r =>
  DStatus.OnStatusBuildReq ->
  m OnStatus.OnStatusMessage
buildOnStatusMessage (DStatus.NewBookingBuildReq {bookingId}) = do
  return $
    OnStatus.OnStatusMessage $
      OnStatus.NewBooking
        NewBookingOS.NewBookingOrder
          { id = bookingId.getId,
            state = NewBookingOS.orderState,
            ..
          }
buildOnStatusMessage (DStatus.RideAssignedBuildReq {bookingId, newRideInfo}) = do
  fulfillment <- buildRideAssignedFulfillment newRideInfo
  return $
    OnStatus.OnStatusMessage $
      OnStatus.RideAssigned
        RideAssignedOS.RideAssignedOrder
          { id = bookingId.getId,
            state = RideAssignedOS.orderState,
            ..
          }
buildOnStatusMessage (DStatus.RideStartedBuildReq {newRideInfo}) = do
  let ride = newRideInfo.ride
  rideAssignedFulfillment <- buildRideAssignedFulfillment newRideInfo
  fulfillment <- buildRideStartedFulfillment ride rideAssignedFulfillment
  return $
    OnStatus.OnStatusMessage $
      OnStatus.RideStarted
        RideStartedOS.RideStartedOrder
          { id = ride.bookingId.getId,
            state = RideStartedOS.orderState,
            fulfillment,
            arrival_time = ride.driverArrivalTime
          }
buildOnStatusMessage (DStatus.RideCompletedBuildReq {newRideInfo, rideCompletedInfo}) = do
  let ride = newRideInfo.ride
      fareParams = rideCompletedInfo.fareParams
      paymentMethodInfo = rideCompletedInfo.paymentMethodInfo
      paymentUrl = rideCompletedInfo.paymentUrl
      arrival_time = ride.driverArrivalTime
  fare <- realToFrac <$> ride.fare & fromMaybeM (InternalError "Ride fare is not present.")
  let currency = "INR"
      price =
        RideCompletedOS.QuotePrice
          { currency,
            value = fare,
            computed_value = fare
          }
      breakup =
        mkBreakupList (RideCompletedOS.BreakupItemPrice currency . fromIntegral) RideCompletedOS.BreakupItem fareParams
          & filter (Common.filterRequiredBreakups $ DFParams.getFareParametersType fareParams) -- TODO: Remove after roll out
  rideAssignedFulfillment <- buildRideAssignedFulfillment newRideInfo
  fulfillment <- buildRideCompletedFulfillment ride rideAssignedFulfillment
  return $
    OnStatus.OnStatusMessage $
      OnStatus.RideCompleted
        RideCompletedOS.RideCompletedOrder
          { id = ride.bookingId.getId,
            state = RideCompletedOS.orderState,
            quote =
              RideCompletedOS.RideCompletedQuote
                { price,
                  breakup
                },
            payment =
              Just
                RideCompletedOS.RideCompletedPayment
                  { collected_by = Common.castDPaymentCollector . (.collectedBy) <$> paymentMethodInfo,
                    _type = Common.castDPaymentType . (.paymentType) <$> paymentMethodInfo,
                    instrument = Common.castDPaymentInstrument . (.paymentInstrument) <$> paymentMethodInfo,
                    time = RideCompletedOS.TimeDuration "FIXME",
                    uri = paymentUrl
                  },
            fulfillment,
            arrival_time
          }
buildOnStatusMessage (DStatus.BookingCancelledBuildReq {mbNewRideInfo, bookingCancelledInfo}) = do
  fulfillment <- forM mbNewRideInfo buildRideAssignedFulfillment
  let booking = bookingCancelledInfo.booking
      cancellationSource = bookingCancelledInfo.cancellationSource
  return $
    OnStatus.OnStatusMessage $
      OnStatus.BookingCancelled
        BookingCancelledOS.BookingCancelledOrder
          { id = booking.id.getId,
            state = BookingCancelledOS.orderState,
            cancellation_reason = Common.castCancellationSource cancellationSource,
            fulfillment = mkBookingCancelledFulfillment <$> fulfillment
          }

buildRideAssignedFulfillment :: (EncFlow m r) => SyncRide.NewRideInfo -> m RideAssignedOS.FulfillmentInfo
buildRideAssignedFulfillment SyncRide.NewRideInfo {..} = do
  mobileNumber <- DP.getPersonNumber driver >>= fromMaybeM (InternalError "Driver mobile number is not present.")
  name <- DP.getPersonFullName driver & fromMaybeM (PersonFieldNotPresent "firstName")
  let agent =
        RideAssignedOS.Agent
          { name = name,
            phone = mobileNumber,
            phoneCountryCode = driver.mobileCountryCode,
            rating = realToFrac <$> driver.rating,
            tags = RideAssignedOS.AgentTags {registered_at = driver.createdAt}
          }
      veh =
        RideAssignedOS.Vehicle
          { model = vehicle.model,
            variant = show vehicle.variant,
            color = vehicle.color,
            registration = vehicle.registrationNo
          }
  pure
    RideAssignedOS.FulfillmentInfo
      { id = ride.id.getId,
        start =
          RideAssignedOS.StartInfo
            { authorization =
                RideAssignedOS.Authorization
                  { _type = "OTP",
                    token = ride.otp
                  }
            },
        agent,
        vehicle = veh,
        ..
      }

buildRideStartedFulfillment :: MonadFlow m => DRide.Ride -> RideAssignedOS.FulfillmentInfo -> m RideStartedOS.FulfillmentInfo
buildRideStartedFulfillment ride RideAssignedOS.FulfillmentInfo {..} = do
  tripStartTime <- ride.tripStartTime & fromMaybeM (RideFieldNotPresent "tripStartTime")
  pure RideStartedOS.FulfillmentInfo {start = mkRideStartedStartInfo tripStartTime start, ..}

buildRideCompletedFulfillment :: MonadFlow m => DRide.Ride -> RideAssignedOS.FulfillmentInfo -> m RideCompletedOS.FulfillmentInfo
buildRideCompletedFulfillment ride RideAssignedOS.FulfillmentInfo {..} = do
  tripStartTime <- ride.tripStartTime & fromMaybeM (RideFieldNotPresent "tripStartTime")
  tripEndTime <- ride.tripEndTime & fromMaybeM (RideFieldNotPresent "tripEndTime")
  chargeableDistance <-
    realToFrac <$> ride.chargeableDistance
      & fromMaybeM (InternalError "Ride chargeable distance is not present.")
  let traveledDistance = realToFrac ride.traveledDistance
  pure
    RideCompletedOS.FulfillmentInfo
      { start = mkRideStartedStartInfo tripStartTime start,
        end =
          RideCompletedOS.EndInfo
            { time = RideCompletedOS.TimeTimestamp tripEndTime
            },
        chargeable_distance = chargeableDistance,
        traveled_distance = traveledDistance,
        ..
      }

mkBookingCancelledFulfillment :: RideAssignedOS.FulfillmentInfo -> BookingCancelledOS.FulfillmentInfo
mkBookingCancelledFulfillment RideAssignedOS.FulfillmentInfo {..} = BookingCancelledOS.FulfillmentInfo {..}

mkRideStartedStartInfo :: UTCTime -> RideAssignedOS.StartInfo -> RideStartedOS.RideStartedStartInfo
mkRideStartedStartInfo tripStartTime RideAssignedOS.StartInfo {..} = do
  RideStartedOS.RideStartedStartInfo {time = RideStartedOS.TimeTimestamp {timestamp = tripStartTime}, ..}
