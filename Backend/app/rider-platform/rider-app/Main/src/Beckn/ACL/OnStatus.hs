{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnStatus (buildOnStatusReq) where

import Beckn.ACL.Common (getTag)
import qualified Beckn.ACL.Common as Common
import qualified Beckn.Types.Core.Taxi.API.OnStatus as OnStatus
import qualified Beckn.Types.Core.Taxi.OnStatus as OnStatus
import Beckn.Types.Core.Taxi.OnStatus.Order.RideAssignedOrder as OnStatusRideAssigned
import qualified Data.Text as T
import qualified Domain.Action.Beckn.OnStatus as DOnStatus
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id (Id (Id))
import Kernel.Utils.Common
import Tools.Error (GenericError (InvalidRequest))

buildOnStatusReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text]
  ) =>
  OnStatus.OnStatusReq ->
  m (Maybe DOnStatus.DOnStatusReq)
buildOnStatusReq req = do
  validateContext Context.ON_STATUS req.context
  handleError req.contents $ \message ->
    parseOrder message.order

parseOrder :: (MonadFlow m) => OnStatus.Order -> m DOnStatus.DOnStatusReq
parseOrder (OnStatus.NewBooking raOrder) = do
  pure
    DOnStatus.DOnStatusReq
      { bppBookingId = Id raOrder.id,
        rideDetails = DOnStatus.NewBookingDetails
      }
parseOrder (OnStatus.RideAssigned raOrder) = do
  newRideInfo <- buildNewRideInfo raOrder.fulfillment
  pure
    DOnStatus.DOnStatusReq
      { bppBookingId = Id raOrder.id,
        rideDetails = DOnStatus.RideAssignedDetails {newRideInfo}
      }
parseOrder (OnStatus.RideStarted rsOrder) = do
  newRideInfo <- buildNewRideInfo rsOrder.fulfillment
  rideStartTime <- fromMaybeM (InvalidRequest "fulfillment.start.time is not present in RideStarted Order.") (rsOrder.fulfillment.start.time <&> (.timestamp))
  tagsGroup <- fromMaybeM (InvalidRequest "agent tags is not present in DriverArrived Event.") rsOrder.fulfillment.tags
  let driverArrivalTime =
        readMaybe . T.unpack
          =<< getTag "driver_arrived_info" "arrival_time" tagsGroup
  let rideStartedInfo =
        DOnStatus.RideStartedInfo
          { rideStartTime,
            driverArrivalTime
          }
  pure
    DOnStatus.DOnStatusReq
      { bppBookingId = Id rsOrder.id,
        rideDetails = DOnStatus.RideStartedDetails {newRideInfo, rideStartedInfo}
      }
parseOrder (OnStatus.RideCompleted rcOrder) = do
  tagsGroup <- fromMaybeM (InvalidRequest "agent tags is not present in RideCompleted Order.") rcOrder.fulfillment.tags
  chargeableDistance :: HighPrecMeters <-
    fromMaybeM (InvalidRequest "chargeable_distance is not present.") $
      readMaybe . T.unpack
        =<< getTag "ride_distance_details" "chargeable_distance" tagsGroup
  traveledDistance :: HighPrecMeters <-
    fromMaybeM (InvalidRequest "traveled_distance is not present.") $
      readMaybe . T.unpack
        =<< getTag "ride_distance_details" "traveled_distance" tagsGroup
  let driverArrivalTime =
        readMaybe . T.unpack
          =<< getTag "driver_arrived_info" "arrival_time" tagsGroup
  newRideInfo <- buildNewRideInfo rcOrder.fulfillment
  rideStartTime <- fromMaybeM (InvalidRequest "fulfillment.start.time is not present in RideCompleted Order.") (rcOrder.fulfillment.start.time <&> (.timestamp))
  rideEndTime <- fromMaybeM (InvalidRequest "fulfillment.end.time is not present in RideCompleted Order.") (rcOrder.fulfillment.end.time <&> (.timestamp))
  let rideStartedInfo =
        DOnStatus.RideStartedInfo
          { rideStartTime,
            driverArrivalTime
          }
  let rideCompletedInfo =
        DOnStatus.RideCompletedInfo
          { rideEndTime,
            fare = roundToIntegral rcOrder.quote.price.value,
            totalFare = roundToIntegral rcOrder.quote.price.computed_value,
            fareBreakups = mkOnStatusFareBreakup <$> rcOrder.quote.breakup,
            chargeableDistance,
            traveledDistance,
            paymentUrl = rcOrder.payment >>= (.uri)
          }
  pure
    DOnStatus.DOnStatusReq
      { bppBookingId = Id rcOrder.id,
        rideDetails = DOnStatus.RideCompletedDetails {newRideInfo, rideStartedInfo, rideCompletedInfo}
      }
  where
    mkOnStatusFareBreakup breakup =
      DOnStatus.OnStatusFareBreakup
        { amount = realToFrac breakup.price.value,
          description = breakup.title
        }
parseOrder (OnStatus.BookingCancelled bcOrder) = do
  mbNewRideInfo <- forM bcOrder.fulfillment buildNewRideInfo
  pure
    DOnStatus.DOnStatusReq
      { bppBookingId = Id bcOrder.id,
        rideDetails =
          DOnStatus.BookingCancelledDetails
            { mbNewRideInfo,
              cancellationSource = Common.castCancellationSource bcOrder.cancellation_reason
            }
      }
parseOrder (OnStatus.BookingReallocation brOrder) = do
  newRideInfo <- buildNewRideInfo brOrder.fulfillment
  pure
    DOnStatus.DOnStatusReq
      { bppBookingId = Id brOrder.id,
        rideDetails =
          DOnStatus.BookingReallocationDetails
            { newRideInfo,
              reallocationSource = Common.castCancellationSource brOrder.reallocation_reason
            }
      }

buildNewRideInfo :: (MonadFlow m) => OnStatusRideAssigned.FulfillmentInfo -> m DOnStatus.NewRideInfo
buildNewRideInfo fulfillment = do
  vehicle <- fromMaybeM (InvalidRequest "vehicle is not present in RideAssigned Order.") $ fulfillment.vehicle
  agent <- fromMaybeM (InvalidRequest "agent is not present in RideAssigned Order.") $ fulfillment.agent
  agentPhone <- fromMaybeM (InvalidRequest "agent phoneNumber is not present in RideAssigned Order.") $ agent.phone
  tagsGroup <- fromMaybeM (InvalidRequest "agent tags is not present in RideAssigned Order.") agent.tags
  registeredAt :: UTCTime <-
    fromMaybeM (InvalidRequest "registered_at is not present.") $
      readMaybe . T.unpack
        =<< getTag "driver_details" "registered_at" tagsGroup
  let rating :: Maybe HighPrecMeters =
        readMaybe . T.unpack
          =<< getTag "driver_details" "rating" tagsGroup
  authorization <- fromMaybeM (InvalidRequest "authorization is not present in RideAssigned Order.") $ fulfillment.start.authorization
  pure
    DOnStatus.NewRideInfo
      { bppRideId = Id fulfillment.id,
        otp = authorization.token,
        driverName = agent.name,
        driverMobileNumber = agentPhone,
        driverMobileCountryCode = Just "+91", -----------TODO needs to be added in agent Tags------------
        driverRating = realToFrac <$> rating,
        driverImage = agent.image,
        driverRegisteredAt = registeredAt,
        vehicleNumber = vehicle.registration,
        vehicleColor = vehicle.color,
        vehicleModel = vehicle.model
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
