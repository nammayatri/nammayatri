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
import qualified Beckn.OnDemand.Utils.Common
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import qualified BecknV2.OnDemand.Utils.Context as ContextV2
import qualified BecknV2.Utils as Utils
import qualified Domain.Action.Beckn.OnUpdate as DOnUpdate
import EulerHS.Prelude hiding (state)
import Kernel.Prelude hiding (find, map, readMaybe)
import qualified Kernel.Types.App
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Error
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
  handleErrorV2 req $ \message -> do
    parseEventV2 transactionId messageId message.confirmReqMessageOrder req.onUpdateReqContext

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

parseEventV2 :: (MonadFlow m, CacheFlow m r) => Text -> Text -> Spec.Order -> Spec.Context -> m DOnUpdate.OnUpdateReq
parseEventV2 transactionId messageId order context = do
  eventType <-
    order.orderFulfillments
      >>= listToMaybe
      >>= (.fulfillmentState)
      >>= (.fulfillmentStateDescriptor)
      >>= (.descriptorCode)
      & fromMaybeM (InvalidRequest "Event type is not present in OnUpdateReq.")

  -- TODO::Beckn, fix this codes after correct v2-spec mapping
  case eventType of
    "RIDE_ASSIGNED" -> do
      assignedReq <- Common.parseRideAssignedEvent order messageId
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
    "NEW_MESSAGE" -> parseNewMessageEvent order
    "SAFETY_ALERT" -> parseSafetyAlertEvent order
    "STOP_ARRIVED" -> parseStopArrivedEvent order
    "UPDATED_ESTIMATE" -> parseUpdatedEstimateEvent transactionId order context
    _ -> throwError $ InvalidRequest $ "Invalid event type: " <> eventType

parseNewMessageEvent :: (MonadFlow m) => Spec.Order -> m DOnUpdate.OnUpdateReq
parseNewMessageEvent order = do
  bppBookingId <- order.orderId & fromMaybeM (InvalidRequest "order_id is not present in NewMessage Event.")
  bppRideId <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentId) & fromMaybeM (InvalidRequest "fulfillment_id is not present in NewMessage Event.")
  tagGroups <- order.orderFulfillments >>= listToMaybe >>= (.fulfillmentTags) & fromMaybeM (InvalidRequest "fulfillment.tags is not present in NewMessage Event.")
  message <- Utils.getTagV2 Tag.DRIVER_NEW_MESSAGE Tag.MESSAGE (Just tagGroups) & fromMaybeM (InvalidRequest "driver_new_message tag is not present in NewMessage Event.")
  return $
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
  deviation <- Utils.getTagV2 Tag.SAFETY_ALERT Tag.DEVIATION (Just tagGroups) & fromMaybeM (InvalidRequest "safety_alert tag is not present in SafetyAlert Event.")
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

parseUpdatedEstimateEvent :: (MonadFlow m) => Text -> Spec.Order -> Spec.Context -> m DOnUpdate.OnUpdateReq
parseUpdatedEstimateEvent transactionId order context = do
  providerId <- order.orderProvider >>= (.providerId) & fromMaybeM (InvalidRequest "fulfillment.provider_id is not present in UpdatedEstimate Event.")
  providerUrl <- Beckn.OnDemand.Utils.Common.getContextBppUri context >>= Kernel.Utils.Error.fromMaybeM (Tools.Error.InvalidRequest "Missing bpp_uri")
  provider <- order.orderProvider & fromMaybeM (InvalidRequest "fulfillment.provider is not present in UpdatedEstimate Event.")
  item <- order.orderItems & fromMaybeM (InvalidRequest "item is not present in UpdatedEstimate Event.")
  fulfillment <- order.orderFulfillments & fromMaybeM (InvalidRequest "fulfillment is not present in UpdatedEstimate Event.")
  name_ <- getProviderName order
  (estimateInfo, quoteInfo) <- partitionEithers <$> traverse (buildEstimateOrQuoteInfo provider fulfillment) item
  let providerInfo =
        DOnUpdate.ProviderInfo
          { providerId = providerId,
            name = name_,
            url = providerUrl,
            mobileNumber = "",
            ridesCompleted = 0
          }
  return $
    DOnUpdate.UpdatedEstimateReq
      { requestId = Id transactionId,
        providerInfo,
        estimateInfo,
        quoteInfo
      }

buildEstimateOrQuoteInfo ::
  (Monad m, Kernel.Types.App.MonadFlow m) =>
  Spec.Provider ->
  [Spec.Fulfillment] ->
  Spec.Item ->
  m (Either DOnUpdate.EstimateInfo DOnUpdate.QuoteInfo)
buildEstimateOrQuoteInfo provider fulfillments item = do
  let descriptions_ = []
  let discount_ = Nothing
  estimatedFare_ <- Utils.getEstimatedFare item
  estimatedTotalFare_ <- Utils.getEstimatedFare item
  itemId_ <- Utils.getItemId item
  specialLocationTag_ <- Utils.buildSpecialLocationTag item
  vehicleVariant_ <- Utils.getVehicleVariant provider item
  quoteOrEstId_ <- Utils.getQuoteFulfillmentId item
  fulfillment <- filter (\f -> f.fulfillmentId == Just quoteOrEstId_) fulfillments & listToMaybe & Kernel.Utils.Error.fromMaybeM (Tools.Error.InvalidRequest "Missing fulfillment for item")
  fulfillmentType <- fulfillment.fulfillmentType & Kernel.Utils.Error.fromMaybeM (Tools.Error.InvalidRequest "Missing fulfillment type")
  case fulfillmentType of
    "RIDE_OTP" -> do
      let quoteDetails_ = DOnUpdate.OneWaySpecialZoneDetails (DOnUpdate.OneWaySpecialZoneQuoteDetails {quoteId = quoteOrEstId_})
      pure $ Right $ DOnUpdate.QuoteInfo {descriptions = descriptions_, discount = discount_, estimatedFare = estimatedFare_, estimatedTotalFare = estimatedTotalFare_, itemId = itemId_, quoteDetails = quoteDetails_, specialLocationTag = specialLocationTag_, vehicleVariant = vehicleVariant_}
    "INTER_CITY" -> do
      let quoteDetails_ = DOnUpdate.InterCityDetails (DOnUpdate.InterCityQuoteDetails {quoteId = quoteOrEstId_})
      pure $ Right $ DOnUpdate.QuoteInfo {descriptions = descriptions_, discount = discount_, estimatedFare = estimatedFare_, estimatedTotalFare = estimatedTotalFare_, itemId = itemId_, quoteDetails = quoteDetails_, specialLocationTag = specialLocationTag_, vehicleVariant = vehicleVariant_}
    _ -> do
      let bppEstimateId_ = Id quoteOrEstId_
      let nightShiftInfo_ = Utils.buildNightShiftInfo item
      totalFareRange_ <- Utils.getTotalFareRange item
      -- waitingCharges_ <- Utils.buildWaitingChargeInfo item
      estimateBreakupList_ <- Utils.buildEstimateBreakupList item
      pure $ Left $ DOnUpdate.EstimateInfo {bppEstimateRevisedId = bppEstimateId_, descriptions = descriptions_, discount = discount_, estimateBreakupList = estimateBreakupList_, estimatedFare = estimatedFare_, estimatedTotalFare = estimatedTotalFare_, itemId = itemId_, nightShiftInfo = nightShiftInfo_, specialLocationTag = specialLocationTag_, totalFareRange = totalFareRange_, vehicleVariant = vehicleVariant_}

getProviderName :: MonadFlow m => Spec.Order -> m Text
getProviderName order =
  order
    & (.orderProvider)
    >>= (.providerDescriptor)
    >>= (.descriptorName)
    & fromMaybeM (InvalidRequest "Missing Provider Name")
