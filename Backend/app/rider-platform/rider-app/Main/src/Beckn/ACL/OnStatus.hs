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
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.DecimalValue (DecimalValue, valueFromString)
import Kernel.Types.Id (Id (Id))
import Kernel.Utils.Common
import Tools.Error (GenericError (InvalidRequest))

buildOnStatusReqV2 ::
  ( HasFlowEnv m r '["_version" ::: Text],
    MonadFlow m,
    CacheFlow m r
  ) =>
  Spec.OnStatusReq ->
  m (Maybe DOnStatus.DOnStatusReq)
buildOnStatusReqV2 req = do
  ContextV2.validateContext Context.ON_STATUS req.onStatusReqContext
  handleErrorV2 req \message -> do
    let order = message.confirmReqMessageOrder
    messageId <- Utils.getMessageId req.onStatusReqContext
    bppBookingIdText <- order.orderId & fromMaybeM (InvalidRequest "order.id is not present in on_status request.")
    let bppBookingId = Id bppBookingIdText
    orderStatus <- order.orderStatus & fromMaybeM (InvalidRequest "order.status is not present in on_status request.")
    eventType <-
      order.orderFulfillments
        >>= listToMaybe
        >>= (.fulfillmentState)
        >>= (.fulfillmentStateDescriptor)
        >>= (.descriptorCode)
        & fromMaybeM (InvalidRequest "Event type is not present in OnUpdateReq.")

    rideDetails <-
      -- TODO::Beckn, need to refactor this codes, according to spec.
      case orderStatus of
        "NEW_BOOKING" -> pure DOnStatus.NewBookingDetails
        "RIDE_BOOKING_REALLOCATION" -> parseRideBookingReallocationOrder order messageId
        "ACTIVE" -> do
          case eventType of
            "RIDE_ASSIGNED" -> do
              assignedReq <- Common.parseRideAssignedEvent order messageId
              return $ DOnStatus.RideAssignedDetails assignedReq
            "RIDE_ARRIVED_PICKUP" -> do
              arrivedReq <- Common.parseDriverArrivedEvent order messageId
              return $ DOnStatus.DriverArrivedDetails arrivedReq
            "RIDE_STARTED" -> do
              startedReq <- Common.parseRideStartedEvent order messageId
              return $ DOnStatus.RideStartedDetails startedReq
            _ -> throwError $ InvalidRequest $ "Invalid event type: " <> eventType
        "COMPLETED" -> do
          case eventType of
            "RIDE_ENDED" -> do
              completedReq <- Common.parseRideCompletedEvent order messageId
              return $ DOnStatus.RideCompletedDetails completedReq
            _ -> throwError $ InvalidRequest $ "Invalid event type: " <> eventType
        "CANCELLED" -> do
          case eventType of
            "RIDE_CANCELLED" -> do
              cancelledReq <- Common.parseBookingCancelledEvent order messageId
              return $ DOnStatus.BookingCancelledDetails cancelledReq
            _ -> throwError $ InvalidRequest $ "Invalid event type: " <> eventType
        _ -> throwError . InvalidRequest $ "Invalid order.status: " <> show orderStatus
    pure $
      DOnStatus.DOnStatusReq
        { bppBookingId,
          rideDetails
        }

parseRideBookingReallocationOrder :: (MonadFlow m, CacheFlow m r) => Spec.Order -> Text -> m DOnStatus.RideDetails
parseRideBookingReallocationOrder order messageId = do
  bookingDetails <- Common.parseBookingDetails order messageId
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
        =<< Utils.getTagV2 Tag.DRIVER_DETAILS Tag.REGISTERED_AT tagGroups
  let rating :: Maybe HighPrecMeters =
        readMaybe . T.unpack
          =<< Utils.getTagV2 Tag.DRIVER_DETAILS Tag.RATING tagGroups
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
          =<< Utils.getTagV2 Tag.DRIVER_ARRIVED_INFO Tag.ARRIVAL_TIME tagGroups
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
        =<< Utils.getTagV2 Tag.RIDE_DISTANCE_DETAILS Tag.CHARGEABLE_DISTANCE tagGroups
  traveledDistance :: HighPrecMeters <-
    fromMaybeM (InvalidRequest "traveled_distance tag is not present in ride_distance_details tagGroup") $
      readMaybe . T.unpack
        =<< Utils.getTagV2 Tag.RIDE_DISTANCE_DETAILS Tag.TRAVELED_DISTANCE tagGroups
  fare :: DecimalValue <- order.orderQuote >>= (.quotationPrice) >>= (.priceValue) >>= valueFromString & fromMaybeM (InvalidRequest "order.quote.price.value is not present in on_status RideCompletedOrder request.")
  totalFare :: DecimalValue <- order.orderQuote >>= (.quotationPrice) >>= (.priceComputedValue) >>= valueFromString & fromMaybeM (InvalidRequest "order.quote.price.computed_value is not present in on_status RideCompletedOrder request.")
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
      amount :: DecimalValue <- breakup.quotationBreakupInnerPrice >>= (.priceValue) >>= valueFromString & fromMaybeM (InvalidRequest "breakup.price.value is not present in on_status RideCompletedOrder request.")
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
