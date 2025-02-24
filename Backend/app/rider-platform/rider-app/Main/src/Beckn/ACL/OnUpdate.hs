{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnUpdate
  ( buildOnUpdateReqV2,
  )
where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified BecknV2.Utils as Utils
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Domain.Action.Beckn.OnUpdate as DOnUpdate
import EulerHS.Prelude hiding (state)
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.DecimalValue as DecimalValue
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Error (GenericError (InvalidRequest))

buildOnUpdateReqV2 ::
  ( HasFlowEnv m r '["_version" ::: Text],
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Spec.OnUpdateReq ->
  m (Maybe DOnUpdate.OnUpdateReq)
buildOnUpdateReqV2 req = do
  ContextV2.validateContext Context.ON_UPDATE $ req.onUpdateReqContext
  transactionId <- Utils.getTransactionId req.onUpdateReqContext
  messageId <- Utils.getMessageId req.onUpdateReqContext
  handleErrorV2 req messageId $ \message -> do
    parseEventV2 transactionId messageId message.confirmReqMessageOrder

handleErrorV2 ::
  (MonadFlow m) =>
  Spec.OnUpdateReq ->
  Text ->
  (Spec.ConfirmReqMessage -> m DOnUpdate.OnUpdateReq) ->
  m (Maybe DOnUpdate.OnUpdateReq)
handleErrorV2 req messageId action = do
  case req.onUpdateReqMessage of
    Just onUpdMsg -> Just <$> action onUpdMsg
    Nothing -> case req.onUpdateReqError of
      Just err -> do
        logTagError "on_update req" $ "on_update error: " <> show err
        case (err.errorMessage, err.errorCode) of
          (Just errorMessage, Just errorCode) ->
            return $ Just $ DOnUpdate.OUEditDestError DOnUpdate.EditDestErrorReq {messageId, errorMessage, errorCode}
          (_, _) -> pure Nothing
      Nothing -> do
        logTagError "on_update req" "on_update message object is not present without any error."
        pure Nothing

parseEventV2 :: (MonadFlow m, CacheFlow m r) => Text -> Text -> Spec.Order -> m DOnUpdate.OnUpdateReq
parseEventV2 transactionId messageId order = do
  case order.orderStatus of
    Just "SOFT_UPDATE" -> do
      editDestinationReq <- parseEditDestinationSoftUpdate order messageId
      return editDestinationReq
    Just "CONFIRM_UPDATE" -> do
      confirmUpdateReq <- parseEditDestinationConfirmUpdate order messageId
      return confirmUpdateReq
    _ -> do
      eventType <-
        order.orderFulfillments
          >>= listToMaybe
          >>= (.fulfillmentState)
          >>= (.fulfillmentStateDescriptor)
          >>= (.descriptorCode)
          & fromMaybeM (InvalidRequest "Event type is not present in OnUpdateReq.")

      -- TODO::Beckn, fix this codes after correct v2-spec mapping
      case eventType of
        "SCHEDULED_RIDE_ASSIGNED" -> do
          assignedReq <- Common.parseRideAssignedEvent order messageId transactionId
          return $ DOnUpdate.OUScheduledRideAssignedReq assignedReq
        "RIDE_ASSIGNED" -> do
          assignedReq <- Common.parseRideAssignedEvent order messageId transactionId
          return $ DOnUpdate.OURideAssignedReq assignedReq
        "RIDE_ARRIVED_PICKUP" -> do
          arrivedReq <- Common.parseDriverArrivedEvent order messageId
          return $ DOnUpdate.OUDriverArrivedReq arrivedReq
        "RIDE_STARTED" -> do
          startedReq <- Common.parseRideStartedEvent order messageId
          return $ DOnUpdate.OURideStartedReq startedReq
        "RIDE_ENDED" -> do
          completedReq <- Common.parseRideCompletedEvent order messageId
          return $ DOnUpdate.OURideCompletedReq completedReq
        "RIDE_CANCELLED" -> do
          cancelledReq <- Common.parseBookingCancelledEvent order messageId
          return $ DOnUpdate.OUBookingCancelledReq cancelledReq
        "ESTIMATE_REPETITION" -> parseEstimateRepetitionEvent transactionId order
        "QUOTE_REPETITION" -> parseQuoteRepetitionEvent transactionId order
        "NEW_MESSAGE" -> parseNewMessageEvent order
        "SAFETY_ALERT" -> parseSafetyAlertEvent order
        "PHONE_CALL_REQUEST" -> return $ DOnUpdate.OUPhoneCallRequestEventReq $ DOnUpdate.PhoneCallRequestEventReq transactionId
        "PHONE_CALL_COMPLETED" -> return $ DOnUpdate.OUPhoneCallCompletedEventReq $ DOnUpdate.PhoneCallCompletedEventReq transactionId
        "STOP_ARRIVED" -> parseStopArrivedEvent order
        "TOLL_CROSSED" -> return $ DOnUpdate.OUTollCrossedEventReq $ DOnUpdate.TollCrossedEventReq transactionId
        "DRIVER_REACHED_DESTINATION" -> parseDriverReachedDestinationEvent order
        "ESTIMATED_END_TIME_RANGE_UPDATED" -> parseEstimatedEndTimeRangeUpdatedEvent order
        "PARCEL_IMAGE_UPLOADED" -> parseParcelImageUploaded order
        _ -> throwError $ InvalidRequest $ "Invalid event type: " <> eventType

parseNewMessageEvent :: (MonadFlow m) => Spec.Order -> m DOnUpdate.OnUpdateReq
parseNewMessageEvent order = do
  bppBookingId <- order.orderId & fromMaybeM (InvalidRequest "order_id is not present in NewMessage Event.")
  bppRideId <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentId) & fromMaybeM (InvalidRequest "fulfillment_id is not present in NewMessage Event.")
  tagGroups <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentTags) & fromMaybeM (InvalidRequest "fulfillment.tags is not present in NewMessage Event.")
  message <- Utils.getTagV2 Tag.DRIVER_NEW_MESSAGE Tag.MESSAGE (Just tagGroups) & fromMaybeM (InvalidRequest "driver_new_message tag is not present in NewMessage Event.")
  return $
    DOnUpdate.OUNewMessageReq $
      DOnUpdate.NewMessageReq
        { bppBookingId = Id bppBookingId,
          bppRideId = Id bppRideId,
          message = message
        }

parseEstimateRepetitionEvent :: (MonadFlow m) => Text -> Spec.Order -> m DOnUpdate.OnUpdateReq
parseEstimateRepetitionEvent transactionId order = do
  bppEstimateId <- order.orderItems >>= listToMaybe >>= (.itemId) & fromMaybeM (InvalidRequest "order_id is not present in EstimateRepetition Event.")
  bppBookingId <- order.orderId & fromMaybeM (InvalidRequest "order_id is not present in EstimateRepetition Event.")
  bppRideId <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentId) & fromMaybeM (InvalidRequest "fulfillment_id is not present in EstimateRepetition Event.")
  tagGroups <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentTags) & fromMaybeM (InvalidRequest "fulfillment.tags is not present in EstimateRepetition Event.")
  cancellationSource <- Utils.getTagV2 Tag.PREVIOUS_CANCELLATION_REASONS Tag.CANCELLATION_REASON (Just tagGroups) & fromMaybeM (InvalidRequest "previous_cancellation_reasons tag is not present in EstimateRepetition Event.")
  return $
    DOnUpdate.OUEstimateRepetitionReq
      DOnUpdate.EstimateRepetitionReq
        { searchRequestId = Id transactionId,
          bppEstimateId = Id bppEstimateId,
          bppBookingId = Id bppBookingId,
          bppRideId = Id bppRideId,
          cancellationSource = Utils.castCancellationSourceV2 cancellationSource
        }

parseQuoteRepetitionEvent :: (MonadFlow m) => Text -> Spec.Order -> m DOnUpdate.OnUpdateReq
parseQuoteRepetitionEvent transactionId order = do
  newBppBookingId <- order.orderItems >>= listToMaybe >>= (.itemId) & fromMaybeM (InvalidRequest "order_id is not present in QuoteRepetition Event.")
  bppBookingId <- order.orderId & fromMaybeM (InvalidRequest "order_id is not present in QuoteRepetition Event.")
  bppRideId <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentId) & fromMaybeM (InvalidRequest "fulfillment_id is not present in QuoteRepetition Event.")
  tagGroups <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentTags) & fromMaybeM (InvalidRequest "fulfillment.tags is not present in QuoteRepetition Event.")
  cancellationSource <- Utils.getTagV2 Tag.PREVIOUS_CANCELLATION_REASONS Tag.CANCELLATION_REASON (Just tagGroups) & fromMaybeM (InvalidRequest "previous_cancellation_reasons tag is not present in QuoteRepetition Event.")
  return $
    DOnUpdate.OUQuoteRepetitionReq
      DOnUpdate.QuoteRepetitionReq
        { searchRequestId = Id transactionId,
          newBppBookingId = Id newBppBookingId,
          bppBookingId = Id bppBookingId,
          bppRideId = Id bppRideId,
          cancellationSource = Utils.castCancellationSourceV2 cancellationSource
        }

parseSafetyAlertEvent :: (MonadFlow m) => Spec.Order -> m DOnUpdate.OnUpdateReq
parseSafetyAlertEvent order = do
  bppBookingId <- order.orderId & fromMaybeM (InvalidRequest "order_id is not present in SafetyAlert Event.")
  bppRideId <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentId) & fromMaybeM (InvalidRequest "fulfillment_id is not present in SafetyAlert Event.")
  tagGroups <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentTags) & fromMaybeM (InvalidRequest "fulfillment.tags is not present in SafetyAlert Event.")
  deviation <- Utils.getTagV2 Tag.SAFETY_ALERT Tag.DEVIATION (Just tagGroups) & fromMaybeM (InvalidRequest "safety_alert tag is not present in SafetyAlert Event.")
  return $
    DOnUpdate.OUSafetyAlertReq
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
    DOnUpdate.OUStopArrivedReq $
      DOnUpdate.StopArrivedReq
        { bppRideId = Id bppRideId
        }

parseEditDestinationSoftUpdate :: (MonadFlow m, CacheFlow m r) => Spec.Order -> Text -> m DOnUpdate.OnUpdateReq
parseEditDestinationSoftUpdate order messageId = do
  bookingDetails <- Common.parseBookingDetails order messageId
  let tagGroups = order.orderFulfillments >>= listToMaybe >>= (.fulfillmentTags)
      personTagsGroup = order.orderFulfillments >>= listToMaybe >>= (.fulfillmentAgent) >>= (.agentPerson) >>= (.personTags)
      currentPoint = Common.getLocationFromTagV2 personTagsGroup Tag.CURRENT_LOCATION Tag.CURRENT_LOCATION_LAT Tag.CURRENT_LOCATION_LON
  newEstimatedDistance :: HighPrecMeters <- Utils.getTagV2 Tag.UPDATE_DETAILS Tag.UPDATED_ESTIMATED_DISTANCE tagGroups >>= readMaybe . T.unpack & fromMaybeM (InvalidRequest "updated_estimated_distance tag is not present in Soft Update Event.")
  fareBreakups' <- order.orderQuote >>= (.quotationBreakup) & fromMaybeM (InvalidRequest "Quote breakup is not present in Soft Update Event.")
  fare :: DecimalValue.DecimalValue <- order.orderQuote >>= (.quotationPrice) >>= (.priceValue) >>= DecimalValue.valueFromString & fromMaybeM (InvalidRequest "quote.price.value is not present in Soft Update Event.")
  currency :: Currency <- order.orderQuote >>= (.quotationPrice) >>= (.priceCurrency) >>= (readMaybe . T.unpack) & fromMaybeM (InvalidRequest "quote.price.currency is not present in Soft Update Event.")
  let fareBreakups = mapMaybe Common.mkDFareBreakup fareBreakups'
  return $
    DOnUpdate.OUEditDestSoftUpdateReq $
      DOnUpdate.EditDestSoftUpdateReq
        { bookingUpdateRequestId = Id messageId,
          newEstimatedDistance,
          fareBreakups,
          fare = Utils.decimalValueToPrice currency fare,
          ..
        }

parseEditDestinationConfirmUpdate :: (MonadFlow m, CacheFlow m r) => Spec.Order -> Text -> m DOnUpdate.OnUpdateReq
parseEditDestinationConfirmUpdate order messageId = do
  bookingDetails <- Common.parseBookingDetails order messageId
  return $
    DOnUpdate.OUEditDestConfirmUpdateReq $
      DOnUpdate.EditDestConfirmUpdateReq
        { bookingUpdateRequestId = Id messageId,
          ..
        }

parseDriverReachedDestinationEvent :: (MonadFlow m) => Spec.Order -> m DOnUpdate.OnUpdateReq
parseDriverReachedDestinationEvent order = do
  bppRideId <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentId) & fromMaybeM (InvalidRequest "fulfillment_id is not present in DestinationReached Event.")
  tagGroups <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentTags) & fromMaybeM (InvalidRequest "fulfillment.tags is not present in DestinationReached Event.")
  destinationReachedTime :: UTCTime <-
    Utils.getTagV2 Tag.DRIVER_REACHED_DESTINATION_INFO Tag.DRIVER_REACHED_DESTINATION (Just tagGroups)
      >>= readMaybe . T.unpack & fromMaybeM (InvalidRequest "DRIVER_REACHED_DESTINATION tag is not present in DestinationReached Event.")
  return $
    DOnUpdate.OUDestinationReachedReq $
      DOnUpdate.DestinationReachedReq
        { bppRideId = Id bppRideId,
          ..
        }

parseEstimatedEndTimeRangeUpdatedEvent :: (MonadFlow m) => Spec.Order -> m DOnUpdate.OnUpdateReq
parseEstimatedEndTimeRangeUpdatedEvent order = do
  bppRideId <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentId) & fromMaybeM (InvalidRequest "fulfillment_id is not present in DestinationReached Event.")
  tagGroups <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentTags) & fromMaybeM (InvalidRequest "fulfillment.tags is not present in DestinationReached Event.")
  estimatedEndTimeRangeStart :: UTCTime <-
    Utils.getTagV2 Tag.ESTIMATED_END_TIME_RANGE Tag.ESTIMATED_END_TIME_RANGE_START (Just tagGroups)
      >>= readMaybe . T.unpack & fromMaybeM (InvalidRequest "ESTIMATED_END_TIME_RANGE_START tag is not present in EstimatedEndTimeRangeUpdated Event.")
  estimatedEndTimeRangeEnd :: UTCTime <-
    Utils.getTagV2 Tag.ESTIMATED_END_TIME_RANGE Tag.ESTIMATED_END_TIME_RANGE_END (Just tagGroups)
      >>= readMaybe . T.unpack & fromMaybeM (InvalidRequest "ESTIMATED_END_TIME_RANGE_END tag is not present in EstimatedEndTimeRangeUpdated Event.")
  return $
    DOnUpdate.OUEstimatedEndTimeRangeReq $
      DOnUpdate.EstimatedEndTimeRangeReq
        { bppRideId = Id bppRideId,
          ..
        }

parseParcelImageUploaded :: (MonadFlow m) => Spec.Order -> m DOnUpdate.OnUpdateReq
parseParcelImageUploaded order = do
  bppRideId <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentId) & fromMaybeM (InvalidRequest "fulfillment_id is not present in DestinationReached Event.")
  tagGroups <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentTags) & fromMaybeM (InvalidRequest "fulfillment.tags is not present in DestinationReached Event.")
  isParcelImageUploaded :: Bool <-
    Utils.getTagV2 Tag.DELIVERY Tag.PARCEL_IMAGE_UPLOADED (Just tagGroups)
      >>= readMaybe . T.unpack & fromMaybeM (InvalidRequest "PARCEL_IMAGE_UPLOADED tag is not present in ParcelImage Event.")
  return $
    DOnUpdate.OUParcelImageFileUploadReq $
      DOnUpdate.ParcelImageFileUploadReq {bppRideId = Id bppRideId, ..}
