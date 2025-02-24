{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.OnDemand.Transformer.OnUpdate
  ( buildOnUpdateReqV2,
    mkOnUpdateMessageV2,
  )
where

import qualified Beckn.ACL.Common.Order as Common
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.OnDemand.Utils.OnUpdate as UtilsOU
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType as Event
import qualified BecknV2.OnDemand.Enums as EventEnum
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils (computeTtlISO8601, mapServiceTierToCategory)
import qualified BecknV2.OnDemand.Utils.Context as CU
import qualified Data.List as List
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.OnUpdate as OU
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Utils.Common
import SharedLogic.Beckn.Common
import qualified SharedLogic.FarePolicy as SFP
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.Queries.FareParameters as QFP

buildOnUpdateReqV2 ::
  (MonadFlow m, EncFlow m r, CacheFlow m r, EsqDBFlow m r) =>
  Context.Action ->
  Context.Domain ->
  Text ->
  Text ->
  BaseUrl ->
  Context.City ->
  Context.Country ->
  DRB.Booking ->
  OU.OnUpdateBuildReq ->
  m Spec.OnUpdateReq
buildOnUpdateReqV2 action domain messageId bppSubscriberId bppUri city country booking req = do
  becknConfig <- QBC.findByMerchantIdDomainAndVehicle booking.providerId "MOBILITY" (Utils.mapServiceTierToCategory booking.vehicleServiceTier) >>= fromMaybeM (InternalError "Beckn Config not found")
  ttl <- becknConfig.onUpdateTTLSec & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
  bapUri <- Kernel.Prelude.parseBaseUrl booking.bapUri
  context <- CU.buildContextV2 action domain messageId (Just booking.transactionId) booking.bapId bapUri (Just bppSubscriberId) (Just bppUri) city country (Just ttl)
  farePolicy <- SFP.getFarePolicyByEstOrQuoteIdWithoutFallback booking.quoteId
  message <- mkOnUpdateMessageV2 req farePolicy becknConfig
  pure $
    Spec.OnUpdateReq
      { onUpdateReqError = Nothing,
        onUpdateReqContext = context,
        onUpdateReqMessage = message
      }

mkOnUpdateMessageV2 ::
  (MonadFlow m, EncFlow m r, CacheFlow m r, EsqDBFlow m r) =>
  OU.OnUpdateBuildReq ->
  Maybe FarePolicyD.FullFarePolicy ->
  DBC.BecknConfig ->
  m (Maybe Spec.ConfirmReqMessage)
mkOnUpdateMessageV2 req mbFarePolicy becknConfig = do
  order <- buildOnUpdateReqOrderV2 req mbFarePolicy becknConfig
  pure . Just $
    Spec.ConfirmReqMessage
      { confirmReqMessageOrder = order
      }

buildOnUpdateReqOrderV2 ::
  (MonadFlow m, EncFlow m r, CacheFlow m r, EsqDBFlow m r) =>
  OU.OnUpdateBuildReq ->
  Maybe FarePolicyD.FullFarePolicy ->
  DBC.BecknConfig ->
  m Spec.Order
buildOnUpdateReqOrderV2 req' mbFarePolicy becknConfig = case req' of
  OU.ScheduledRideAssignedBuildReq req -> Common.tfAssignedReqToOrder req mbFarePolicy becknConfig EventEnum.SCHEDULED_RIDE_ASSIGNED
  OU.RideAssignedBuildReq req -> Common.tfAssignedReqToOrder req mbFarePolicy becknConfig EventEnum.RIDE_ASSIGNED
  OU.RideStartedBuildReq req -> Common.tfStartReqToOrder req mbFarePolicy becknConfig
  OU.RideCompletedBuildReq req -> Common.tfCompleteReqToOrder req mbFarePolicy becknConfig
  OU.BookingCancelledBuildReq req -> Common.tfCancelReqToOrder req becknConfig
  OU.DriverArrivedBuildReq req -> Common.tfArrivedReqToOrder req mbFarePolicy becknConfig
  OU.DriverReachedDestinationBuildReq req -> Common.tfReachedDestinationReqToOrder req
  OU.EstimateRepetitionBuildReq OU.DEstimateRepetitionReq {..} -> do
    let BookingDetails {..} = bookingDetails
    let previousCancellationReasonsTags = UtilsOU.mkPreviousCancellationReasonsTags cancellationSource
    fulfillment <- Utils.mkFulfillmentV2 Nothing Nothing ride booking Nothing Nothing previousCancellationReasonsTags Nothing False False Nothing (Just $ show Event.ESTIMATE_REPETITION) isValueAddNP Nothing False 0 -- TODO::Beckn, decide on fulfillment.state.descriptor.code mapping according to spec-v2
    pure $
      Spec.Order
        { orderId = Just booking.id.getId,
          orderFulfillments = Just [fulfillment],
          orderItems =
            Just . List.singleton $
              Spec.Item
                { itemId = Just estimateId.getId,
                  itemDescriptor = Nothing,
                  itemFulfillmentIds = Nothing,
                  itemLocationIds = Nothing,
                  itemPaymentIds = Nothing,
                  itemPrice = Nothing,
                  itemTags = Utils.mkRateCardTag Nothing Nothing booking.estimatedFare booking.fareParams.congestionChargeViaDp (Just . FarePolicyD.fullFarePolicyToFarePolicy =<< mbFarePolicy) Nothing Nothing
                },
          orderBilling = Nothing,
          orderCancellation = Nothing,
          orderCancellationTerms = Nothing,
          orderPayments = Utils.tfPayments booking bookingDetails.merchant becknConfig,
          orderProvider = Nothing,
          orderQuote = Nothing,
          orderStatus = Nothing,
          orderCreatedAt = Just booking.createdAt,
          orderUpdatedAt = Just booking.updatedAt
        }
  OU.NewMessageBuildReq OU.DNewMessageReq {..} -> do
    let BookingDetails {..} = bookingDetails
    let newMessageTags = UtilsOU.mkNewMessageTags message
    fulfillment <- Utils.mkFulfillmentV2 (Just driver) (Just driverStats) ride booking (Just vehicle) Nothing newMessageTags Nothing False False Nothing (Just $ show Event.NEW_MESSAGE) isValueAddNP Nothing False 0 -- TODO::Beckn, decide on fulfillment.state.descriptor.code mapping according to spec-v2
    pure $
      Spec.Order
        { orderId = Just ride.bookingId.getId,
          orderFulfillments = Just [fulfillment],
          orderBilling = Nothing,
          orderCancellation = Nothing,
          orderCancellationTerms = Nothing,
          orderItems = Nothing,
          orderPayments = Nothing,
          orderProvider = Nothing,
          orderQuote = Nothing,
          orderStatus = Nothing,
          orderCreatedAt = Just booking.createdAt,
          orderUpdatedAt = Just booking.updatedAt
        }
  OU.SafetyAlertBuildReq OU.DSafetyAlertReq {..} -> do
    let BookingDetails {..} = bookingDetails
    let safetyAlertTags = UtilsOU.mkSafetyAlertTags reason
    fulfillment <- Utils.mkFulfillmentV2 Nothing Nothing ride booking Nothing Nothing safetyAlertTags Nothing False False Nothing (Just $ show Event.SAFETY_ALERT) isValueAddNP Nothing False 0 -- TODO::Beckn, decide on fulfillment.state.descriptor.code mapping according to spec-v2
    pure $
      Spec.Order
        { orderId = Just ride.bookingId.getId,
          orderFulfillments = Just [fulfillment],
          orderBilling = Nothing,
          orderCancellation = Nothing,
          orderCancellationTerms = Nothing,
          orderItems = Nothing,
          orderPayments = Nothing,
          orderProvider = Nothing,
          orderQuote = Nothing,
          orderStatus = Nothing,
          orderCreatedAt = Just booking.createdAt,
          orderUpdatedAt = Just booking.updatedAt
        }
  OU.PhoneCallRequestBuildReq OU.DPhoneCallRequestReq {..} -> do
    let BookingDetails {..} = bookingDetails
    fulfillment <- Utils.mkFulfillmentV2 Nothing Nothing ride booking Nothing Nothing Nothing Nothing False False Nothing (Just $ show Event.PHONE_CALL_REQUEST) isValueAddNP Nothing False 0
    pure $
      Spec.Order
        { orderId = Just ride.bookingId.getId,
          orderFulfillments = Just [fulfillment],
          orderBilling = Nothing,
          orderCancellation = Nothing,
          orderCancellationTerms = Nothing,
          orderItems = Nothing,
          orderPayments = Nothing,
          orderProvider = Nothing,
          orderQuote = Nothing,
          orderStatus = Nothing,
          orderCreatedAt = Just booking.createdAt,
          orderUpdatedAt = Just booking.updatedAt
        }
  OU.PhoneCallCompletedBuildReq OU.DPhoneCallCompletedReq {..} -> do
    let BookingDetails {..} = bookingDetails
    fulfillment <- Utils.mkFulfillmentV2 Nothing Nothing ride booking Nothing Nothing Nothing Nothing False False Nothing (Just $ show Event.PHONE_CALL_COMPLETED) isValueAddNP Nothing False 0
    pure $
      Spec.Order
        { orderId = Just ride.bookingId.getId,
          orderFulfillments = Just [fulfillment],
          orderBilling = Nothing,
          orderCancellation = Nothing,
          orderCancellationTerms = Nothing,
          orderItems = Nothing,
          orderPayments = Nothing,
          orderProvider = Nothing,
          orderQuote = Nothing,
          orderStatus = Nothing,
          orderCreatedAt = Just booking.createdAt,
          orderUpdatedAt = Just booking.updatedAt
        }
  OU.StopArrivedBuildReq OU.DStopArrivedBuildReq {..} -> do
    let BookingDetails {..} = bookingDetails
    fulfillment <- Utils.mkFulfillmentV2 Nothing Nothing ride booking Nothing Nothing Nothing Nothing False False Nothing (Just $ show Event.STOP_ARRIVED) isValueAddNP Nothing False 0
    pure $
      Spec.Order
        { orderId = Just ride.bookingId.getId,
          orderFulfillments = Just [fulfillment],
          orderBilling = Nothing,
          orderCancellation = Nothing,
          orderCancellationTerms = Nothing,
          orderItems = Nothing,
          orderPayments = Nothing,
          orderProvider = Nothing,
          orderQuote = Nothing,
          orderStatus = Nothing,
          orderCreatedAt = Just booking.createdAt,
          orderUpdatedAt = Just booking.updatedAt
        }
  OU.EditDestinationUpdate OU.DEditDestinationUpdateReq {..} -> do
    let BookingDetails {..} = bookingDetails
    fareParameters <- QFP.findById bookingUpdateReqDetails.fareParamsId >>= fromMaybeM (InternalError "Fare Parameters not found")
    let farePolicy = FarePolicyD.fullFarePolicyToFarePolicy <$> mbFarePolicy
    let (items, orderStatus, quote, payment) = case updateType of
          OU.SOFT_UPDATE -> do
            let items' = Utils.tfItemsSoftUpdate booking merchant.shortId.getShortId bookingUpdateReqDetails.estimatedDistance farePolicy Nothing bookingUpdateReqDetails ride.id.getId
                orderStatus' = Just $ show EventEnum.SOFT_UPDATE
                quote' = Utils.tfQuotationSU fareParameters bookingUpdateReqDetails.estimatedFare
                payment' = UtilsOU.mkPaymentParamsSoftUpdate paymentMethodInfo paymentUrl merchant bppConfig bookingUpdateReqDetails.estimatedFare fareParameters.currency
            (items', orderStatus', quote', payment')
          OU.CONFIRM_UPDATE -> do
            let items' = Utils.tfItems booking merchant.shortId.getShortId booking.estimatedDistance farePolicy Nothing
                orderStatus' = Just $ show EventEnum.CONFIRM_UPDATE
                quote' = Utils.tfQuotation booking
                payment' = UtilsOU.mkPaymentParams paymentMethodInfo paymentUrl merchant bppConfig booking
            (items', orderStatus', quote', payment')
    fulfillment <- case updateType of
      OU.SOFT_UPDATE -> do
        newDestination' <- newDestination & fromMaybeM (InternalError "New Destination not found for SOFT UPDATE")
        let updateDetailsTagGroup = if isValueAddNP then UtilsOU.mkUpdatedDistanceTags bookingUpdateReqDetails.estimatedDistance else Nothing
            personTag = if isValueAddNP then Utils.mkLocationTagGroupV2 currentLocation else Nothing
        Utils.mkFulfillmentV2SoftUpdate (Just driver) (Just driverStats) ride booking (Just vehicle) Nothing updateDetailsTagGroup personTag False False Nothing Nothing isValueAddNP newDestination' False 0
      OU.CONFIRM_UPDATE -> Utils.mkFulfillmentV2 (Just driver) (Just driverStats) ride booking (Just vehicle) Nothing Nothing Nothing False False Nothing Nothing isValueAddNP Nothing False 0
    pure $
      Spec.Order
        { orderId = Just $ booking.id.getId,
          orderStatus,
          orderFulfillments = Just [fulfillment],
          orderBilling = Nothing,
          orderCancellation = Nothing,
          orderCancellationTerms = Nothing,
          orderItems = items,
          orderPayments = Just [payment],
          orderProvider = Utils.tfProvider becknConfig,
          orderQuote = quote,
          orderCreatedAt = Just booking.createdAt,
          orderUpdatedAt = Just booking.updatedAt
        }
  OU.QuoteRepetitionBuildReq OU.DQuoteRepetitionReq {..} -> do
    let BookingDetails {..} = bookingDetails
    let previousCancellationReasonsTags = UtilsOU.mkPreviousCancellationReasonsTags cancellationSource
    fulfillment <- Utils.mkFulfillmentV2 Nothing Nothing ride booking Nothing Nothing previousCancellationReasonsTags Nothing False False Nothing (Just $ show Event.QUOTE_REPETITION) isValueAddNP Nothing False 0
    pure $
      Spec.Order
        { orderId = Just booking.id.getId,
          orderFulfillments = Just [fulfillment],
          orderItems =
            Just . List.singleton $
              Spec.Item
                { itemId = Just newBookingId.getId,
                  itemDescriptor = Nothing,
                  itemFulfillmentIds = Nothing,
                  itemLocationIds = Nothing,
                  itemPaymentIds = Nothing,
                  itemPrice = Nothing,
                  itemTags = Utils.mkRateCardTag Nothing Nothing booking.estimatedFare booking.fareParams.congestionChargeViaDp (Just . FarePolicyD.fullFarePolicyToFarePolicy =<< mbFarePolicy) Nothing Nothing
                },
          orderBilling = Nothing,
          orderCancellation = Nothing,
          orderCancellationTerms = Nothing,
          orderPayments = Utils.tfPayments booking bookingDetails.merchant becknConfig,
          orderProvider = Nothing,
          orderQuote = Nothing,
          orderStatus = Nothing,
          orderCreatedAt = Just booking.createdAt,
          orderUpdatedAt = Just booking.updatedAt
        }
  OU.TollCrossedBuildReq OU.DTollCrossedBuildReq {..} -> do
    let BookingDetails {..} = bookingDetails
    fulfillment <- Utils.mkFulfillmentV2 Nothing Nothing ride booking Nothing Nothing Nothing Nothing False False Nothing (Just $ show Event.TOLL_CROSSED) isValueAddNP Nothing False 0
    pure $
      Spec.Order
        { orderId = Just ride.bookingId.getId,
          orderFulfillments = Just [fulfillment],
          orderBilling = Nothing,
          orderCancellation = Nothing,
          orderCancellationTerms = Nothing,
          orderItems = Nothing,
          orderPayments = Nothing,
          orderProvider = Nothing,
          orderQuote = Nothing,
          orderStatus = Nothing,
          orderCreatedAt = Just booking.createdAt,
          orderUpdatedAt = Just booking.updatedAt
        }
  OU.RideEstimatedEndTimeRangeBuildReq OU.DRideEstimatedEndTimeRangeReq {..} -> do
    let BookingDetails {..} = bookingDetails
    let estimatedEndTimeRangeTagGroup = Utils.mkEstimatedEndTimeRangeTagGroupV2 ride.estimatedEndTimeRange
    fulfillment <- Utils.mkFulfillmentV2 Nothing Nothing ride booking Nothing Nothing estimatedEndTimeRangeTagGroup Nothing False False Nothing (Just $ show Event.ESTIMATED_END_TIME_RANGE_UPDATED) isValueAddNP Nothing False 0
    pure $
      Spec.Order
        { orderId = Just ride.bookingId.getId,
          orderFulfillments = Just [fulfillment],
          orderBilling = Nothing,
          orderCancellation = Nothing,
          orderCancellationTerms = Nothing,
          orderItems = Nothing,
          orderPayments = Nothing,
          orderProvider = Nothing,
          orderQuote = Nothing,
          orderStatus = Nothing,
          orderCreatedAt = Just booking.createdAt,
          orderUpdatedAt = Just booking.updatedAt
        }
  OU.ParcelImageUploadedBuildReq OU.DParcelImageUploadedReq {..} -> do
    let BookingDetails {..} = bookingDetails
    let parcelImageUploadedTag = Utils.mkParcelImageUploadedTag
    fulfillment <- Utils.mkFulfillmentV2 Nothing Nothing ride booking Nothing Nothing parcelImageUploadedTag Nothing False False Nothing (Just $ show Event.PARCEL_IMAGE_UPLOADED) isValueAddNP Nothing False 0
    pure $
      Spec.Order
        { orderId = Just ride.bookingId.getId,
          orderFulfillments = Just [fulfillment],
          orderBilling = Nothing,
          orderCancellation = Nothing,
          orderCancellationTerms = Nothing,
          orderItems = Nothing,
          orderPayments = Nothing,
          orderProvider = Nothing,
          orderQuote = Nothing,
          orderStatus = Nothing,
          orderCreatedAt = Just booking.createdAt,
          orderUpdatedAt = Just booking.updatedAt
        }
