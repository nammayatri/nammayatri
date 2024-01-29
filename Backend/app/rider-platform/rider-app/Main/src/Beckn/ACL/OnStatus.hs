{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnStatus (buildOnStatusReq, buildOnStatusReqV2) where

import Beckn.ACL.Common (getTag)
import qualified Beckn.ACL.Common as Common
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.API.OnStatus as OnStatus
import qualified Beckn.Types.Core.Taxi.OnStatus as OnStatus
import Beckn.Types.Core.Taxi.OnStatus.Order.RideAssignedOrder as OnStatusRideAssigned
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified Data.Text as T
import qualified Domain.Action.Beckn.OnStatus as DOnStatus
import Kernel.Prelude
import Kernel.Product.Validation.Context
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.DecimalValue (DecimalValue)
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

buildOnStatusReqV2 ::
  ( HasFlowEnv m r '["_version" ::: Text],
    MonadFlow m
  ) =>
  Spec.OnStatusReq ->
  m (Maybe DOnStatus.DOnStatusReq)
buildOnStatusReqV2 req = do
  ContextV2.validateContext Context.ON_STATUS req.onStatusReqContext
  handleErrorV2 req \message -> do
    let order = message.confirmReqMessageOrder
    bppBookingIdText <- order.orderId & fromMaybeM (InvalidRequest "order.id is not present in on_status request.")
    let bppBookingId = Id bppBookingIdText
    orderStatus <- order.orderStatus & fromMaybeM (InvalidRequest "order.status is not present in on_status request.")
    rideDetails <-
      -- TODO::Beckn, need to refactor this codes, according to spec.
      case orderStatus of
        "NEW_BOOKING" -> pure DOnStatus.NewBookingDetails
        "RIDE_ASSIGNED" -> parseRideAssignedOrder order
        "RIDE_STARTED" -> parseRideStartedOrder order
        "RIDE_COMPLETED" -> parseRideCompletedOrder order
        "RIDE_BOOKING_CANCELLED" -> parseRideBookingCancelledOrder order
        "RIDE_BOOKING_REALLOCATION" -> parseRideBookingReallocationOrder order
        _ -> throwError . InvalidRequest $ "Invalid order.status: " <> show orderStatus
    pure $
      DOnStatus.DOnStatusReq
        { bppBookingId,
          rideDetails
        }

parseRideAssignedOrder :: (MonadFlow m) => Spec.Order -> m DOnStatus.RideDetails
parseRideAssignedOrder order = do
  newRideInfo <- buildNewRideInfoV2 order
  pure $ DOnStatus.RideAssignedDetails {newRideInfo}

parseRideStartedOrder :: (MonadFlow m) => Spec.Order -> m DOnStatus.RideDetails
parseRideStartedOrder order = do
  newRideInfo <- buildNewRideInfoV2 order
  rideStartedInfo <- buildRideStartedInfo order
  pure $ DOnStatus.RideStartedDetails {..}

parseRideCompletedOrder :: (MonadFlow m) => Spec.Order -> m DOnStatus.RideDetails
parseRideCompletedOrder order = do
  newRideInfo <- buildNewRideInfoV2 order
  rideStartedInfo <- buildRideStartedInfo order
  rideCompletedInfo <- buildRideCompletedInfo order
  pure $ DOnStatus.RideCompletedDetails {..}

parseRideBookingCancelledOrder :: (MonadFlow m) => Spec.Order -> m DOnStatus.RideDetails
parseRideBookingCancelledOrder order = do
  mbNewRideInfo <-
    case order.orderFulfillments of
      Just _ -> Just <$> buildNewRideInfoV2 order
      Nothing -> pure Nothing
  cancellationSourceText <- order.orderCancellation >>= (.cancellationCancelledBy) & fromMaybeM (InvalidRequest "order.cancellation.cancelled_by is not present in on_status BookingCancelledEvent request.")
  let cancellationSource = Utils.castCancellationSourceV2 cancellationSourceText
  pure $ DOnStatus.BookingCancelledDetails {..}

parseRideBookingReallocationOrder :: (MonadFlow m) => Spec.Order -> m DOnStatus.RideDetails
parseRideBookingReallocationOrder order = do
  newRideInfo <- buildNewRideInfoV2 order
  reallocationSourceText <- order.orderCancellation >>= (.cancellationCancelledBy) & fromMaybeM (InvalidRequest "order.cancellation.,cancelled_by is not present in on_status BookingReallocationEvent request.")
  let reallocationSource = Utils.castCancellationSourceV2 reallocationSourceText
  pure $ DOnStatus.BookingReallocationDetails {..}

buildNewRideInfoV2 :: (MonadFlow m) => Spec.Order -> m DOnStatus.NewRideInfo
buildNewRideInfoV2 order = do
  fulf <- order.orderFulfillments >>= listToMaybe & fromMaybeM (InvalidRequest "order.fulfillments is not present in on_status request.")
  fulfillmentId <- fulf.fulfillmentId & fromMaybeM (InvalidRequest "fulfillment.id is not present in on_status request.")
  rideOtp <- fulf.fulfillmentStops >>= Utils.getStartLocation >>= (.stopAuthorization) >>= (.authorizationToken) & fromMaybeM (InvalidRequest "fulfillment.stops.authorization.token for start location is not present in on_status request.")
  agent <- fulf.fulfillmentAgent & fromMaybeM (InvalidRequest "fulfillment.agent is not present in on_status request.")
  driverName <- agent.agentPerson >>= (.personName) & fromMaybeM (InvalidRequest "agent.person.name is not present in on_status request.")
  let driverImage = agent.agentPerson >>= (.personImage) >>= (.imageUrl)
  driverMobileNumber <- agent.agentContact >>= (.contactPhone) & fromMaybeM (InvalidRequest "agent.contact.phone is not present in on_status request.")
  tagGroups <- agent.agentPerson >>= (.personTags) & fromMaybeM (InvalidRequest "agent.person.tags is not present in on_status request.")
  driverRegisteredAt :: UTCTime <-
    fromMaybeM (InvalidRequest "registered_at tag not present in driver_details tagGroups of agent.person.tags") $
      readMaybe . T.unpack
        =<< Utils.getTagV2 "driver_details" "registered_at" tagGroups
  let rating :: Maybe HighPrecMeters =
        readMaybe . T.unpack
          =<< Utils.getTagV2 "driver_details" "rating" tagGroups
  vehicle <- fulf.fulfillmentVehicle & fromMaybeM (InvalidRequest "fulfillment.vehicle is not present in on_status request.")
  vehicleNumber <- vehicle.vehicleRegistration & fromMaybeM (InvalidRequest "vehicle.registration is not present in on_status request.")
  vehicleColor <- vehicle.vehicleColor & fromMaybeM (InvalidRequest "vehicle.color is not present in on_status request.")
  vehicleModel <- vehicle.vehicleModel & fromMaybeM (InvalidRequest "vehicle.model is not present in on_status request.")
  pure $
    DOnStatus.NewRideInfo
      { bppRideId = Id fulfillmentId,
        otp = rideOtp,
        driverName,
        driverMobileNumber,
        driverMobileCountryCode = Just "+91", -----------TODO needs to be added in agent Tags------------
        driverRating = realToFrac <$> rating,
        driverImage,
        driverRegisteredAt,
        vehicleNumber,
        vehicleColor,
        vehicleModel
      }

buildRideStartedInfo :: (MonadFlow m) => Spec.Order -> m DOnStatus.RideStartedInfo
buildRideStartedInfo order = do
  fulf <- order.orderFulfillments >>= listToMaybe & fromMaybeM (InvalidRequest "order.fulfillments is not present in on_status request.")
  rideStartTime <- fulf.fulfillmentStops >>= Utils.getStartLocation >>= (.stopTime) >>= (.timeTimestamp) & fromMaybeM (InvalidRequest "fulfillment.stops.time.timestamp for start location is not present in on_status request.")
  tagGroups <- fulf.fulfillmentTags & fromMaybeM (InvalidRequest "fulfillment.tags is not present in on_status request.")
  let driverArrivalTime :: Maybe UTCTime =
        readMaybe . T.unpack
          =<< Utils.getTagV2 "driver_arrived_info" "arrival_time" tagGroups
  pure $
    DOnStatus.RideStartedInfo
      { rideStartTime,
        driverArrivalTime
      }

buildRideCompletedInfo :: (MonadFlow m) => Spec.Order -> m DOnStatus.RideCompletedInfo
buildRideCompletedInfo order = do
  fulf <- order.orderFulfillments >>= listToMaybe & fromMaybeM (InvalidRequest "order.fulfillments is not present in on_status request.")
  rideEndTime <- fulf.fulfillmentStops >>= Utils.getDropLocation >>= (.stopTime) >>= (.timeTimestamp) & fromMaybeM (InvalidRequest "fulfillment.stops.time.timestamp for drop location is not present in on_status request.")
  tagGroups <- fulf.fulfillmentTags & fromMaybeM (InvalidRequest "fulfillment.tags is not present in on_status request.")
  chargeableDistance :: HighPrecMeters <-
    fromMaybeM (InvalidRequest "chargeable_distance tag is not present in ride_distance_details tagGroup") $
      readMaybe . T.unpack
        =<< Utils.getTagV2 "ride_distance_details" "chargeable_distance" tagGroups
  traveledDistance :: HighPrecMeters <-
    fromMaybeM (InvalidRequest "traveled_distance tag is not present in ride_distance_details tagGroup") $
      readMaybe . T.unpack
        =<< Utils.getTagV2 "ride_distance_details" "traveled_distance" tagGroups
  fare :: DecimalValue <- order.orderQuote >>= (.quotationPrice) >>= (.priceValue) >>= readMaybe . T.unpack & fromMaybeM (InvalidRequest "order.quote.price.value is not present in on_status RideCompletedOrder request.")
  totalFare :: DecimalValue <- order.orderQuote >>= (.quotationPrice) >>= (.priceComputedValue) >>= readMaybe . T.unpack & fromMaybeM (InvalidRequest "order.quote.price.computed_value is not present in on_status RideCompletedOrder request.")
  breakup <- order.orderQuote >>= (.quotationBreakup) & fromMaybeM (InvalidRequest "order.quote.breakup is not present in on_status RideCompletedOrder request.")
  fareBreakups <- traverse mkOnStatusFareBreakupV2 breakup
  pure
    DOnStatus.RideCompletedInfo
      { rideEndTime,
        fare = roundToIntegral fare,
        totalFare = roundToIntegral totalFare,
        fareBreakups,
        chargeableDistance,
        traveledDistance,
        paymentUrl = Nothing
      }
  where
    mkOnStatusFareBreakupV2 breakup = do
      amount :: DecimalValue <- breakup.quotationBreakupInnerPrice >>= (.priceValue) >>= readMaybe . T.unpack & fromMaybeM (InvalidRequest "breakup.price.value is not present in on_status RideCompletedOrder request.")
      description <- breakup.quotationBreakupInnerTitle & fromMaybeM (InvalidRequest "breakup.title is not present in on_status RideCompletedOrder request.")
      pure
        DOnStatus.OnStatusFareBreakup
          { amount = realToFrac amount,
            description
          }

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
