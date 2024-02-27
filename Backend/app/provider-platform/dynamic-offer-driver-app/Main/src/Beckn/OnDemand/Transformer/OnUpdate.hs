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
import qualified BecknV2.OnDemand.Utils.Context as CU
import qualified Data.List as List
import Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Booking as DRB
import Domain.Types.Merchant as DM
import qualified Domain.Types.OnUpdate as OU
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Utils.Common
import SharedLogic.Beckn.Common

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
  DBC.BecknConfig ->
  DM.Merchant ->
  m Spec.OnUpdateReq
buildOnUpdateReqV2 action domain messageId bppSubscriberId bppUri city country booking req bppConfig merchant = do
  context <- CU.buildContextV2 action domain messageId (Just booking.transactionId) booking.bapId booking.bapUri (Just bppSubscriberId) (Just bppUri) city country
  message <- mkOnUpdateMessageV2 req bppConfig merchant
  pure $
    Spec.OnUpdateReq
      { onUpdateReqError = Nothing,
        onUpdateReqContext = context,
        onUpdateReqMessage = message
      }

mkOnUpdateMessageV2 ::
  (MonadFlow m, EncFlow m r, CacheFlow m r, EsqDBFlow m r) =>
  OU.OnUpdateBuildReq ->
  DBC.BecknConfig ->
  DM.Merchant ->
  m (Maybe Spec.ConfirmReqMessage)
mkOnUpdateMessageV2 req bppConfig merchant = do
  order <- buildOnUpdateReqOrderV2 req bppConfig merchant
  pure . Just $
    Spec.ConfirmReqMessage
      { confirmReqMessageOrder = order
      }

buildOnUpdateReqOrderV2 ::
  (MonadFlow m, EncFlow m r, CacheFlow m r, EsqDBFlow m r) =>
  OU.OnUpdateBuildReq ->
  DBC.BecknConfig ->
  DM.Merchant ->
  m Spec.Order
buildOnUpdateReqOrderV2 req' bppConfig merchant = case req' of
  OU.RideAssignedBuildReq req -> Common.tfAssignedReqToOrder req
  OU.RideStartedBuildReq req -> Common.tfStartReqToOrder req
  OU.RideCompletedBuildReq req -> Common.tfCompleteReqToOrder req bppConfig merchant
  OU.BookingCancelledBuildReq req -> Common.tfCancelReqToOrder req
  OU.DriverArrivedBuildReq req -> Common.tfArrivedReqToOrder req
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
                  itemTags = Nothing
                },
          orderBilling = Nothing,
          orderCancellation = Nothing,
          orderCancellationTerms = Nothing,
          orderPayments = Nothing,
          orderProvider = Nothing,
          orderQuote = Nothing,
          orderStatus = Nothing
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
          orderStatus = Nothing
        }
  OU.SafetyAlertBuildReq OU.DSafetyAlertReq {..} -> do
    let BookingDetails {..} = bookingDetails
    let safetyAlertTags = Utils.mkSafetyAlertTags reason code
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
          orderStatus = Nothing
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
          orderStatus = Nothing
        }
  OU.UpdatedEstimateBuildReq OU.DUpdatedEstimateReq {..} -> do
    logDebug $ "hello world recieved in transformer"
    let BookingDetails {..} = bookingDetails
    let updatedEstimateTags = Utils.mkUpdatedEstimateTags estimateRevised
    fulfillment <- Utils.mkFulfillmentV2 Nothing ride booking Nothing Nothing updatedEstimateTags Nothing False False (Just $ show Event.UPDATED_ESTIMATE) isValueAddNP -- TODO::Beckn, decide on fulfillment.state.descriptor.code mapping according to spec-v2
    logDebug $ "hello world sent from transformer"
    pure $
      Spec.Order
        { orderId = Just ride.bookingId.getId,
          orderFulfillments = Just [fulfillment],
          orderBilling = Nothing,
          orderCancellation = Nothing,
          orderCancellationTerms = Nothing,
          orderItems =
            Just $
              List.singleton $
                Spec.Item
                  { itemId = Just estimateRevised.id.getId,
                    itemDescriptor = Nothing,
                    itemFulfillmentIds = Nothing,
                    itemLocationIds = Nothing,
                    itemPaymentIds = Nothing,
                    itemPrice = Nothing,
                    itemTags = Nothing
                  },
          orderPayments = Nothing,
          orderProvider =
            Just $
              Spec.Provider
                { providerId = Just provider.id.getId,
                  providerLocations = Nothing,
                  providerPayments = Nothing,
                  providerDescriptor = Nothing,
                  providerItems = Nothing,
                  providerFulfillments = Nothing
                },
          orderQuote = Nothing,
          orderStatus = Nothing
        }
