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
  )
where

import qualified Beckn.ACL.Common.Order as Common
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.OnDemand.Utils.OnUpdate as Utils
import qualified Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.OnUpdateEventType as Event
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils (computeTtlISO8601)
import qualified BecknV2.OnDemand.Utils.Context as CU
import qualified Data.List as List
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.OnUpdate as OU
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Error
import Kernel.Utils.Common
import SharedLogic.Beckn.Common
import qualified SharedLogic.FarePolicy as SFP
import qualified Storage.CachedQueries.BecknConfig as QBC

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
  context <- CU.buildContextV2 action domain messageId (Just booking.transactionId) booking.bapId booking.bapUri (Just bppSubscriberId) (Just bppUri) city country (Just ttl)
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
  OU.RideAssignedBuildReq req -> Common.tfAssignedReqToOrder req mbFarePolicy becknConfig
  OU.RideStartedBuildReq req -> Common.tfStartReqToOrder req mbFarePolicy becknConfig
  OU.RideCompletedBuildReq req -> Common.tfCompleteReqToOrder req mbFarePolicy becknConfig
  OU.BookingCancelledBuildReq req -> Common.tfCancelReqToOrder req becknConfig
  OU.DriverArrivedBuildReq req -> Common.tfArrivedReqToOrder req mbFarePolicy becknConfig
  OU.EstimateRepetitionBuildReq OU.DEstimateRepetitionReq {..} -> do
    let BookingDetails {..} = bookingDetails
    let previousCancellationReasonsTags = Utils.mkPreviousCancellationReasonsTags cancellationSource
    fulfillment <- Utils.mkFulfillmentV2 Nothing ride booking Nothing Nothing previousCancellationReasonsTags Nothing False False (Just $ show Event.ESTIMATE_REPETITION) isValueAddNP -- TODO::Beckn, decide on fulfillment.state.descriptor.code mapping according to spec-v2
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
                  itemTags = Utils.mkRateCardTag Nothing . Just . FarePolicyD.fullFarePolicyToFarePolicy =<< mbFarePolicy
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
    let newMessageTags = Utils.mkNewMessageTags message
    fulfillment <- Utils.mkFulfillmentV2 (Just driver) ride booking (Just vehicle) Nothing newMessageTags Nothing False False (Just $ show Event.NEW_MESSAGE) isValueAddNP -- TODO::Beckn, decide on fulfillment.state.descriptor.code mapping according to spec-v2
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
    let safetyAlertTags = Utils.mkSafetyAlertTags reason
    fulfillment <- Utils.mkFulfillmentV2 Nothing ride booking Nothing Nothing safetyAlertTags Nothing False False (Just $ show Event.SAFETY_ALERT) isValueAddNP -- TODO::Beckn, decide on fulfillment.state.descriptor.code mapping according to spec-v2
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
    fulfillment <- Utils.mkFulfillmentV2 Nothing ride booking Nothing Nothing Nothing Nothing False False (Just $ show Event.STOP_ARRIVED) isValueAddNP
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
