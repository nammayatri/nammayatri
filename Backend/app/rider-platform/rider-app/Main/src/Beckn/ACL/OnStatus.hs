{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnStatus (buildOnStatusReq) where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.Types.Core.Taxi.API.OnStatus as OnStatus
import qualified Beckn.Types.Core.Taxi.OnStatus as OnStatus
import Beckn.Types.Core.Taxi.OnStatus.Order.BookingCancelledOrder as OnStatusBookingCancelled
import Beckn.Types.Core.Taxi.OnStatus.Order.RideAssignedOrder as OnStatusRideAssigned
import Beckn.Types.Core.Taxi.OnStatus.Order.RideCompletedOrder as OnStatusRideCompleted
import Beckn.Types.Core.Taxi.OnStatus.Order.RideStartedOrder as OnStatusRideStarted
import qualified Domain.Action.Beckn.OnStatus as DOnStatus
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id (Id (Id))
import Kernel.Utils.Common

buildOnStatusReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text]
  ) =>
  OnStatus.OnStatusReq ->
  m (Maybe DOnStatus.DOnStatusReq)
buildOnStatusReq req = do
  validateContext Context.ON_STATUS req.context
  handleError req.contents $ \message ->
    return $ parseOrder message.order

parseOrder :: OnStatus.Order -> DOnStatus.DOnStatusReq
parseOrder (OnStatus.NewBooking raOrder) = do
  DOnStatus.DOnStatusReq
    { bppBookingId = Id raOrder.id,
      rideDetails = DOnStatus.NewBookingDetails
    }
parseOrder (OnStatus.RideAssigned raOrder) = do
  let newRideInfo = mkNewRideInfo raOrder.fulfillment
  DOnStatus.DOnStatusReq
    { bppBookingId = Id raOrder.id,
      rideDetails = DOnStatus.RideAssignedDetails {newRideInfo}
    }
parseOrder (OnStatus.RideStarted rsOrder) = do
  let newRideInfo = mkNewRideInfo . mkRideAssignedFulfillment $ rsOrder.fulfillment
  let rideStartedInfo =
        DOnStatus.RideStartedInfo
          { rideStartTime = rsOrder.fulfillment.start.time.timestamp,
            driverArrivalTime = rsOrder.arrival_time
          }
  DOnStatus.DOnStatusReq
    { bppBookingId = Id rsOrder.id,
      rideDetails = DOnStatus.RideStartedDetails {newRideInfo, rideStartedInfo}
    }
  where
    mkRideAssignedFulfillment OnStatusRideStarted.FulfillmentInfo {..} = OnStatusRideAssigned.FulfillmentInfo {start = mkRideAssignedStartInfo start, ..}
parseOrder (OnStatus.RideCompleted rcOrder) = do
  let newRideInfo = mkNewRideInfo . mkRideAssignedFulfillment $ rcOrder.fulfillment
  let rideStartedInfo =
        DOnStatus.RideStartedInfo
          { rideStartTime = rcOrder.fulfillment.start.time.timestamp,
            driverArrivalTime = rcOrder.arrival_time
          }
  let rideCompletedInfo =
        DOnStatus.RideCompletedInfo
          { rideEndTime = rcOrder.fulfillment.end.time.timestamp,
            fare = roundToIntegral rcOrder.quote.price.value,
            totalFare = roundToIntegral rcOrder.quote.price.computed_value,
            fareBreakups = mkOnStatusFareBreakup <$> rcOrder.quote.breakup,
            chargeableDistance = realToFrac rcOrder.fulfillment.chargeable_distance,
            traveledDistance = realToFrac rcOrder.fulfillment.traveled_distance,
            paymentUrl = rcOrder.payment >>= (.uri)
          }
  DOnStatus.DOnStatusReq
    { bppBookingId = Id rcOrder.id,
      rideDetails = DOnStatus.RideCompletedDetails {newRideInfo, rideStartedInfo, rideCompletedInfo}
    }
  where
    mkRideAssignedFulfillment OnStatusRideCompleted.FulfillmentInfo {..} = OnStatusRideAssigned.FulfillmentInfo {start = mkRideAssignedStartInfo start, ..}
    mkOnStatusFareBreakup breakup =
      DOnStatus.OnStatusFareBreakup
        { amount = realToFrac breakup.price.value,
          description = breakup.title
        }
parseOrder (OnStatus.BookingCancelled bcOrder) = do
  let mbNewRideInfo = mkNewRideInfo . mkRideAssignedFulfillment <$> bcOrder.fulfillment
  DOnStatus.DOnStatusReq
    { bppBookingId = Id bcOrder.id,
      rideDetails =
        DOnStatus.BookingCancelledDetails
          { mbNewRideInfo,
            cancellationSource = Common.castCancellationSource bcOrder.cancellation_reason
          }
    }
  where
    mkRideAssignedFulfillment OnStatusBookingCancelled.FulfillmentInfo {..} = OnStatusRideAssigned.FulfillmentInfo {..}

mkRideAssignedStartInfo :: OnStatusRideStarted.RideStartedStartInfo -> OnStatusRideAssigned.StartInfo
mkRideAssignedStartInfo OnStatusRideStarted.RideStartedStartInfo {..} = OnStatusRideAssigned.StartInfo {..}

mkNewRideInfo :: OnStatusRideAssigned.FulfillmentInfo -> DOnStatus.NewRideInfo
mkNewRideInfo fulfillment =
  DOnStatus.NewRideInfo
    { bppRideId = Id fulfillment.id,
      otp = fulfillment.start.authorization.token,
      driverName = fulfillment.agent.name,
      driverMobileNumber = fulfillment.agent.phone,
      driverMobileCountryCode = fulfillment.agent.phoneCountryCode,
      driverRating = realToFrac <$> fulfillment.agent.rating,
      driverRegisteredAt = fulfillment.agent.tags.registered_at,
      vehicleNumber = fulfillment.vehicle.registration,
      vehicleColor = fulfillment.vehicle.color,
      vehicleModel = fulfillment.vehicle.model
    }

handleError ::
  (MonadFlow m) =>
  Either Error OnStatus.OnStatusMessage ->
  (OnStatus.OnStatusMessage -> m DOnStatus.DOnStatusReq) ->
  m (Maybe DOnStatus.DOnStatusReq)
handleError etr action =
  case etr of
    Right msg -> do
      Just <$> action msg
    Left err -> do
      logTagError "on_status req" $ "on_status error: " <> show err
      pure Nothing
