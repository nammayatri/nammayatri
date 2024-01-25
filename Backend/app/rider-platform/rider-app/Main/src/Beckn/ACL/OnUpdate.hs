{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnUpdate
  ( buildOnUpdateReq,
    buildOnUpdateReqV2,
  )
where

import Beckn.ACL.Common (getTag)
import qualified Beckn.ACL.Common as Common
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.OnUpdate as OnUpdate
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Domain.Action.Beckn.OnUpdate as DOnUpdate
import EulerHS.Prelude hiding (state)
import Kernel.Prelude (roundToIntegral)
import Kernel.Product.Validation.Context (validateContext)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Beckn.DecimalValue (DecimalValue)
import Kernel.Types.Beckn.ReqTypes (BecknCallbackReq)
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Error (GenericError (InvalidRequest))

buildOnUpdateReq ::
  ( HasFlowEnv m r '["coreVersion" ::: Text],
    EsqDBFlow m r
  ) =>
  BecknCallbackReq OnUpdate.OnUpdateMessage ->
  m (Maybe DOnUpdate.OnUpdateReq)
buildOnUpdateReq req = do
  validateContext Context.ON_UPDATE $ req.context
  transactionId <- req.context.transaction_id & fromMaybeM (InvalidRequest "transaction_id is not present.")
  handleError req.contents $ \message -> do
    parseEvent transactionId message.order

buildOnUpdateReqV2 ::
  ( HasFlowEnv m r '["_version" ::: Text],
    EsqDBFlow m r
  ) =>
  Spec.OnUpdateReq ->
  m (Maybe DOnUpdate.OnUpdateReq)
buildOnUpdateReqV2 req = do
  ContextV2.validateContext Context.ON_UPDATE $ req.onUpdateReqContext
  transactionId <- Utils.getTransactionId req.onUpdateReqContext
  handleErrorV2 req $ \message -> do
    parseEventV2 transactionId message.confirmReqMessageOrder

handleError ::
  (MonadFlow m) =>
  Either Error OnUpdate.OnUpdateMessage ->
  (OnUpdate.OnUpdateMessage -> m DOnUpdate.OnUpdateReq) ->
  m (Maybe DOnUpdate.OnUpdateReq)
handleError etr action =
  case etr of
    Right msg -> do
      Just <$> action msg
    Left err -> do
      logTagError "on_update req" $ "on_update error: " <> show err
      pure Nothing

handleErrorV2 ::
  (MonadFlow m) =>
  Spec.OnUpdateReq ->
  (Spec.ConfirmReqMessage -> m DOnUpdate.OnUpdateReq) ->
  m (Maybe DOnUpdate.OnUpdateReq)
handleErrorV2 req action = do
  onUpdMsg <- req.onUpdateReqMessage & fromMaybeM (InvalidRequest "message not present in on_update request.")
  case req.onUpdateReqError of
    Nothing -> Just <$> action onUpdMsg
    Just err -> do
      logTagError "on_update req" $ "on_update error: " <> show err
      pure Nothing

parseEvent :: (MonadFlow m) => Text -> OnUpdate.OnUpdateEvent -> m DOnUpdate.OnUpdateReq
parseEvent _ (OnUpdate.RideAssigned taEvent) = do
  vehicle <- fromMaybeM (InvalidRequest "vehicle is not present in RideAssigned Event.") $ taEvent.fulfillment.vehicle
  agent <- fromMaybeM (InvalidRequest "agent is not present in RideAssigned Event.") $ taEvent.fulfillment.agent
  agentPhone <- fromMaybeM (InvalidRequest "agent phoneNumber is not present in RideAssigned Event.") $ agent.phone
  tagsGroup <- fromMaybeM (InvalidRequest "agent tags is not present in RideAssigned Event.") agent.tags
  registeredAt :: UTCTime <-
    fromMaybeM (InvalidRequest "registered_at is not present.") $
      readMaybe . T.unpack
        =<< getTag "driver_details" "registered_at" tagsGroup
  let rating :: Maybe HighPrecMeters =
        readMaybe . T.unpack
          =<< getTag "driver_details" "rating" tagsGroup
      mbIsDriverBirthDay = getTag "driver_details" "is_driver_birthday" tagsGroup
      mbIsFreeRide = getTag "driver_details" "is_free_ride" tagsGroup
      isDriverBirthDay = castToBool mbIsDriverBirthDay
      isFreeRide = castToBool mbIsFreeRide
      castToBool mbVar = case T.toLower <$> mbVar of
        Just "true" -> True
        _ -> False
  authorization <- fromMaybeM (InvalidRequest "authorization is not present in RideAssigned Event.") $ taEvent.fulfillment.start.authorization
  return $
    DOnUpdate.RideAssignedReq
      { bppBookingId = Id taEvent.id,
        bppRideId = Id taEvent.fulfillment.id,
        otp = authorization.token,
        driverName = agent.name,
        driverMobileNumber = agentPhone,
        driverMobileCountryCode = Just "+91", -----------TODO needs to be added in agent Tags------------
        driverRating = realToFrac <$> rating,
        driverImage = agent.image,
        driverRegisteredAt = registeredAt,
        isDriverBirthDay,
        isFreeRide,
        vehicleNumber = vehicle.registration,
        vehicleColor = vehicle.color,
        vehicleModel = vehicle.model
      }
parseEvent _ (OnUpdate.RideStarted rsEvent) = do
  return $
    DOnUpdate.RideStartedReq
      { bppBookingId = Id rsEvent.id,
        bppRideId = Id rsEvent.fulfillment.id
      }
parseEvent _ (OnUpdate.RideCompleted rcEvent) = do
  tagsGroup <- fromMaybeM (InvalidRequest "agent tags is not present in RideCompleted Event.") rcEvent.fulfillment.tags
  chargeableDistance :: HighPrecMeters <-
    fromMaybeM (InvalidRequest "chargeable_distance is not present.") $
      readMaybe . T.unpack
        =<< getTag "ride_distance_details" "chargeable_distance" tagsGroup
  traveledDistance :: HighPrecMeters <-
    fromMaybeM (InvalidRequest "traveled_distance is not present.") $
      readMaybe . T.unpack
        =<< getTag "ride_distance_details" "traveled_distance" tagsGroup
  return $
    DOnUpdate.RideCompletedReq
      { bppBookingId = Id rcEvent.id,
        bppRideId = Id rcEvent.fulfillment.id,
        fare = roundToIntegral rcEvent.quote.price.value,
        totalFare = roundToIntegral rcEvent.quote.price.computed_value,
        chargeableDistance = chargeableDistance,
        traveledDistance = traveledDistance,
        fareBreakups = mkOnUpdateFareBreakup <$> rcEvent.quote.breakup,
        paymentUrl = rcEvent.payment >>= (.uri)
      }
  where
    mkOnUpdateFareBreakup breakup =
      DOnUpdate.OnUpdateFareBreakup
        { amount = realToFrac breakup.price.value,
          description = breakup.title
        }
parseEvent _ (OnUpdate.BookingCancelled tcEvent) = do
  return $
    DOnUpdate.BookingCancelledReq
      { bppBookingId = Id $ tcEvent.id,
        cancellationSource = Common.castCancellationSource tcEvent.cancellation_reason
      }
parseEvent _ (OnUpdate.BookingReallocation rbrEvent) = do
  return $
    DOnUpdate.BookingReallocationReq
      { bppBookingId = Id $ rbrEvent.id,
        bppRideId = Id rbrEvent.fulfillment.id,
        reallocationSource = Common.castCancellationSource rbrEvent.reallocation_reason
      }
parseEvent _ (OnUpdate.DriverArrived daEvent) = do
  tagsGroup <- fromMaybeM (InvalidRequest "agent tags is not present in DriverArrived Event.") daEvent.fulfillment.tags
  let arrival_time =
        readMaybe . T.unpack
          =<< getTag "driver_arrived_info" "arrival_time" tagsGroup

  return $
    DOnUpdate.DriverArrivedReq
      { bppBookingId = Id daEvent.id,
        bppRideId = Id daEvent.fulfillment.id,
        arrivalTime = arrival_time
      }
parseEvent _ (OnUpdate.NewMessage daEvent) = do
  tagsGroup <- fromMaybeM (InvalidRequest "agent tags is not present in NewMessage Event.") daEvent.fulfillment.tags
  message :: Text <-
    fromMaybeM (InvalidRequest "message is not present.") $
      getTag "driver_new_message" "message" tagsGroup
  return $
    DOnUpdate.NewMessageReq
      { bppBookingId = Id daEvent.id,
        bppRideId = Id daEvent.fulfillment.id,
        message = message
      }
parseEvent transactionId (OnUpdate.EstimateRepetition erEvent) = do
  tagsGroup <- fromMaybeM (InvalidRequest "agent tags is not present in EstimateRepetition Event.") erEvent.fulfillment.tags
  cancellationReason <-
    fromMaybeM (InvalidRequest "cancellation_reason is not present.") $
      readMaybe . T.unpack
        =<< getTag "previous_cancellation_reasons" "cancellation_reason" tagsGroup
  return $
    DOnUpdate.EstimateRepetitionReq
      { searchRequestId = Id transactionId,
        bppEstimateId = Id erEvent.item.id,
        bppBookingId = Id $ erEvent.id,
        bppRideId = Id erEvent.fulfillment.id,
        cancellationSource = Common.castCancellationSource cancellationReason
      }
parseEvent _ (OnUpdate.SafetyAlert saEvent) = do
  tagsGroup <- fromMaybeM (InvalidRequest "agent tags is not present in SafetyAlert Event.") saEvent.fulfillment.tags
  deviation :: Text <-
    fromMaybeM (InvalidRequest "deviation is not present.") $
      getTag "safety_alert" "deviation" tagsGroup
  return $
    DOnUpdate.SafetyAlertReq
      { bppBookingId = Id saEvent.id,
        bppRideId = Id saEvent.fulfillment.id,
        reason = deviation,
        code = "deviation"
      }
parseEvent _ (OnUpdate.StopArrived saEvent) = do
  return $
    DOnUpdate.StopArrivedReq
      { bppRideId = Id saEvent.fulfillment.id
      }

parseEventV2 :: (MonadFlow m) => Text -> Spec.Order -> m DOnUpdate.OnUpdateReq
parseEventV2 transactionId order = do
  eventType <-
    order.orderFulfillments
      >>= listToMaybe
      >>= (.fulfillmentState)
      >>= (.fulfillmentStateDescriptor)
      >>= (.descriptorCode)
      & fromMaybeM (InvalidRequest "Event type is not present in OnUpdateReq.")

  case eventType of
    "RIDE_ASSIGNED" -> parseRideAssignedEvent order
    "DRIVER_ARRIVED" -> parseDriverArrivedEvent order
    "RIDE_STARTED" -> parseRideStartedEvent order
    "RIDE_COMPLETED" -> parseRideCompletedEvent order
    "RIDE_BOOKING_CANCELLED" -> parseBookingCancelledEvent order
    "ESTIMATE_REPETITION" -> parseEstimateRepetitionEvent transactionId order
    "NEW_MESSAGE" -> parseNewMessageEvent order
    "SAFETY_ALERT" -> parseSafetyAlertEvent order
    "STOP_ARRIVED" -> parseStopArrivedEvent order
    _ -> throwError $ InvalidRequest $ "Invalid event type: " <> eventType

parseRideAssignedEvent :: (MonadFlow m) => Spec.Order -> m DOnUpdate.OnUpdateReq
parseRideAssignedEvent order = do
  bppBookingId <- order.orderId & fromMaybeM (InvalidRequest "order_id is not present in RideAssigned Event.")
  bppRideId <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentId) & fromMaybeM (InvalidRequest "fulfillment_id is not present in RideAssigned Event.")
  stops <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentStops) & fromMaybeM (InvalidRequest "fulfillment_stops is not present in RideAssigned Event.")
  start <- Utils.getStartLocation stops & fromMaybeM (InvalidRequest "pickup stop is not present in RideAssigned Event.")
  otp <- start.stopAuthorization >>= (.authorizationToken) & fromMaybeM (InvalidRequest "authorization_token is not present in RideAssigned Event.")
  driverName <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentAgent) >>= (.agentPerson) >>= (.personName) & fromMaybeM (InvalidRequest "driverName is not present in RideAssigned Event.")
  driverMobileNumber <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentAgent) >>= (.agentContact) >>= (.contactPhone) & fromMaybeM (InvalidRequest "driverMobileNumber is not present in RideAssigned Event.")
  tagGroups <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentAgent) >>= (.agentPerson) >>= (.personTags) & fromMaybeM (InvalidRequest "personTags is not present in RideAssigned Event.")
  let rating :: Maybe HighPrecMeters = readMaybe . T.unpack =<< Utils.getTagV2 "driver_details" "rating" tagGroups
  registeredAt :: UTCTime <- fromMaybeM (InvalidRequest "registered_at is not present.") $ readMaybe . T.unpack =<< Utils.getTagV2 "driver_details" "registered_at" tagGroups
  let driverImage = order.orderFulfillments >>= listToMaybe >>= (.fulfillmentAgent) >>= (.agentPerson) >>= (.personImage) >>= (.imageUrl)
  vehicleColor <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentVehicle) >>= (.vehicleColor) & fromMaybeM (InvalidRequest "vehicleColor is not present in RideAssigned Event.")
  vehicleModel <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentVehicle) >>= (.vehicleModel) & fromMaybeM (InvalidRequest "vehicleModel is not present in RideAssigned Event.")
  vehicleNumber <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentVehicle) >>= (.vehicleRegistration) & fromMaybeM (InvalidRequest "vehicleNumber is not present in RideAssigned Event.")
  let castToBool mbVar = case T.toLower <$> mbVar of
        Just "true" -> True
        _ -> False
  let isDriverBirthDay = castToBool $ Utils.getTagV2 "driver_details" "is_driver_birthday" tagGroups
      isFreeRide = castToBool $ Utils.getTagV2 "driver_details" "is_free_ride" tagGroups
  return
    DOnUpdate.RideAssignedReq
      { bppBookingId = Id bppBookingId,
        bppRideId = Id bppRideId,
        driverMobileCountryCode = Just "+91", -----------TODO needs to be added in agent Tags------------
        driverRating = realToFrac <$> rating,
        driverRegisteredAt = registeredAt,
        otp,
        driverName,
        driverMobileNumber,
        driverImage,
        vehicleNumber,
        vehicleColor,
        vehicleModel,
        isDriverBirthDay,
        isFreeRide
      }

parseRideStartedEvent :: (MonadFlow m) => Spec.Order -> m DOnUpdate.OnUpdateReq
parseRideStartedEvent order = do
  bppBookingId <- order.orderId & fromMaybeM (InvalidRequest "order_id is not present in RideStarted Event.")
  bppRideId <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentId) & fromMaybeM (InvalidRequest "fulfillment_id is not present in RideStarted Event.")
  pure $
    DOnUpdate.RideStartedReq
      { bppBookingId = Id bppBookingId,
        bppRideId = Id bppRideId
      }

parseRideCompletedEvent :: (MonadFlow m) => Spec.Order -> m DOnUpdate.OnUpdateReq
parseRideCompletedEvent order = do
  bppBookingId <- order.orderId & fromMaybeM (InvalidRequest "order_id is not present in RideCompleted Event.")
  bppRideId <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentId) & fromMaybeM (InvalidRequest "fulfillment_id is not present in RideCompleted Event.")
  fare :: DecimalValue <- order.orderQuote >>= (.quotationPrice) >>= (.priceValue) >>= readMaybe & fromMaybeM (InvalidRequest "quote.price.value is not present in RideCompleted Event.")
  totalFare :: DecimalValue <- order.orderQuote >>= (.quotationPrice) >>= (.priceComputedValue) >>= readMaybe & fromMaybeM (InvalidRequest "qoute.price.computed_value is not present in RideCompleted Event.")
  tagGroups <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentTags) & fromMaybeM (InvalidRequest "fulfillment tags is not present in RideCompleted Event.")
  chargeableDistance :: HighPrecMeters <-
    fromMaybeM (InvalidRequest "chargeable_distance is not present in RideCompleted Event.") $
      readMaybe . T.unpack
        =<< Utils.getTagV2 "ride_distance_details" "chargeable_distance" tagGroups
  traveledDistance :: HighPrecMeters <-
    fromMaybeM (InvalidRequest "traveled_distance is not present in RideCompleted Event.") $
      readMaybe . T.unpack
        =<< Utils.getTagV2 "ride_distance_details" "traveled_distance" tagGroups
  fareBreakups' <- order.orderQuote >>= (.quotationBreakup) & fromMaybeM (InvalidRequest "quote breakup is not present in RideCompleted Event.")
  fareBreakups <- traverse mkOnUpdateFareBreakup fareBreakups'
  pure $
    DOnUpdate.RideCompletedReq
      { bppBookingId = Id bppBookingId,
        bppRideId = Id bppRideId,
        fare = roundToIntegral fare,
        totalFare = roundToIntegral totalFare,
        chargeableDistance = chargeableDistance,
        traveledDistance = traveledDistance,
        fareBreakups = fareBreakups,
        paymentUrl = Nothing
      }
  where
    mkOnUpdateFareBreakup breakup = do
      val :: DecimalValue <- breakup.quotationBreakupInnerPrice >>= (.priceValue) >>= readMaybe & fromMaybeM (InvalidRequest "quote.breakup.price.value is not present in RideCompleted Event.")
      title <- breakup.quotationBreakupInnerTitle & fromMaybeM (InvalidRequest "breakup_title is not present in RideCompleted Event.")
      pure $
        DOnUpdate.OnUpdateFareBreakup
          { amount = realToFrac val,
            description = title
          }

parseBookingCancelledEvent :: (MonadFlow m) => Spec.Order -> m DOnUpdate.OnUpdateReq
parseBookingCancelledEvent order = do
  bppBookingId <- order.orderId & fromMaybeM (InvalidRequest "order_id is not present in BookingCancelled Event.")
  cancellationSource <- order.orderCancellation >>= (.cancellationCancelledBy) & fromMaybeM (InvalidRequest "cancellationSource is not present in BookingCancelled Event.")
  return $
    DOnUpdate.BookingCancelledReq
      { bppBookingId = Id bppBookingId,
        cancellationSource = Utils.castCancellationSourceV2 cancellationSource
      }

parseDriverArrivedEvent :: (MonadFlow m) => Spec.Order -> m DOnUpdate.OnUpdateReq
parseDriverArrivedEvent order = do
  bppBookingId <- order.orderId & fromMaybeM (InvalidRequest "order_id is not present in DriverArrived Event.")
  bppRideId <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentId) & fromMaybeM (InvalidRequest "fulfillment_id is not present in DriverArrived Event.")
  tagGroups <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentAgent) >>= (.agentPerson) >>= (.personTags) & fromMaybeM (InvalidRequest "fulfillment.agent.tags is not present in DriverArrived Event.")
  let arrival_time = readMaybe . T.unpack =<< Utils.getTagV2 "driver_arrived_info" "arrival_time" tagGroups
  return $
    DOnUpdate.DriverArrivedReq
      { bppBookingId = Id bppBookingId,
        bppRideId = Id bppRideId,
        arrivalTime = arrival_time
      }

parseNewMessageEvent :: (MonadFlow m) => Spec.Order -> m DOnUpdate.OnUpdateReq
parseNewMessageEvent order = do
  bppBookingId <- order.orderId & fromMaybeM (InvalidRequest "order_id is not present in NewMessage Event.")
  bppRideId <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentId) & fromMaybeM (InvalidRequest "fulfillment_id is not present in NewMessage Event.")
  tagGroups <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentTags) & fromMaybeM (InvalidRequest "fulfillment.tags is not present in NewMessage Event.")
  message <- Utils.getTagV2 "driver_new_message" "message" tagGroups & fromMaybeM (InvalidRequest "driver_new_message tag is not present in NewMessage Event.")
  return $
    DOnUpdate.NewMessageReq
      { bppBookingId = Id bppBookingId,
        bppRideId = Id bppRideId,
        message = message
      }

parseEstimateRepetitionEvent :: (MonadFlow m) => Text -> Spec.Order -> m DOnUpdate.OnUpdateReq
parseEstimateRepetitionEvent transactionId order = do
  bppEstimateId <- order.orderId & fromMaybeM (InvalidRequest "order_id is not present in EstimateRepetition Event.")
  bppBookingId <- order.orderId & fromMaybeM (InvalidRequest "order_id is not present in EstimateRepetition Event.")
  bppRideId <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentId) & fromMaybeM (InvalidRequest "fulfillment_id is not present in EstimateRepetition Event.")
  cancellationSource <- order.orderCancellation >>= (.cancellationCancelledBy) & fromMaybeM (InvalidRequest "order.cancellation.cancelled_by is not present in EstimateRepetition Event.")
  return $
    DOnUpdate.EstimateRepetitionReq
      { searchRequestId = Id transactionId,
        bppEstimateId = Id bppEstimateId,
        bppBookingId = Id bppBookingId,
        bppRideId = Id bppRideId,
        cancellationSource = Utils.castCancellationSourceV2 cancellationSource
      }

parseSafetyAlertEvent :: (MonadFlow m) => Spec.Order -> m DOnUpdate.OnUpdateReq
parseSafetyAlertEvent order = do
  bppBookingId <- order.orderId & fromMaybeM (InvalidRequest "order_id is not present in SafetyAlert Event.")
  bppRideId <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentId) & fromMaybeM (InvalidRequest "fulfillment_id is not present in SafetyAlert Event.")
  tagGroups <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentTags) & fromMaybeM (InvalidRequest "fulfillment.tags is not present in SafetyAlert Event.")
  deviation <- Utils.getTagV2 "safety_alert" "deviation" tagGroups & fromMaybeM (InvalidRequest "safety_alert tag is not present in SafetyAlert Event.")
  return $
    DOnUpdate.SafetyAlertReq
      { bppBookingId = Id bppBookingId,
        bppRideId = Id bppRideId,
        reason = deviation,
        code = "deviation"
      }

parseStopArrivedEvent :: (MonadFlow m) => Spec.Order -> m DOnUpdate.OnUpdateReq
parseStopArrivedEvent order = do
  bppRideId <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentId) & fromMaybeM (InvalidRequest "fulfillment_id is not present in StopArrived Event.")
  return $
    DOnUpdate.StopArrivedReq
      { bppRideId = Id bppRideId
      }
