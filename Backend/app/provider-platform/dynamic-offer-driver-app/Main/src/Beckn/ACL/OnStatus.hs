{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnStatus
  ( buildOnStatusMessage,
    mkOnStatusMessageV2,
    buildOnStatusReqV2,
    module DStatus,
  )
where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.ACL.Common.Order as Common
import qualified Beckn.OnDemand.Utils.Common as BUtils
import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.OnDemand.Utils.OnUpdate as UtilsOU
import qualified Beckn.Types.Core.Taxi.Common.Tags as Tags
import qualified Beckn.Types.Core.Taxi.OnStatus as OnStatus
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.BookingCancelledOrder as BookingCancelledOS
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.BookingReallocationOrder as BookingReallocationOS
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.NewBookingOrder as NewBookingOS
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.RideAssignedOrder as RideAssignedOS
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.RideCompletedOrder as RideCompletedOS
import qualified Beckn.Types.Core.Taxi.OnStatus.Order.RideStartedOrder as RideStartedOS
import qualified BecknV2.OnDemand.Enums as EventEnum
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils (computeTtlISO8601, mapServiceTierToCategory)
import qualified BecknV2.OnDemand.Utils.Context as CU
import Domain.Types.Beckn.Status as DStatus
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.Merchant as DM
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Beckn.Common as Common
import qualified SharedLogic.FarePolicy as SFP
import qualified Storage.CachedQueries.BecknConfig as QBC

buildOnStatusMessage ::
  (EsqDBFlow m r, EncFlow m r) =>
  DStatus.OnStatusBuildReq ->
  m OnStatus.OnStatusMessage
buildOnStatusMessage (DStatus.NewBookingBuildReq (DNewBookingBuildReq bookingId)) = do
  return $
    OnStatus.OnStatusMessage
      { order =
          OnStatus.NewBooking $
            NewBookingOS.NewBookingOrder
              { id = bookingId.getId,
                state = NewBookingOS.orderState,
                ..
              }
      }
buildOnStatusMessage (DStatus.RideAssignedReq Common.DRideAssignedReq {..}) = do
  let Common.BookingDetails {..} = bookingDetails
  let arrivalTimeTagGroup = Common.mkArrivalTimeTagGroup ride.driverArrivalTime
  fulfillment <- Common.mkFulfillment (Just driver) (Just driverStats) ride booking (Just vehicle) image (Just $ Tags.TG arrivalTimeTagGroup) Nothing False False False 0 booking.isSafetyPlus
  return $
    OnStatus.OnStatusMessage
      { order =
          OnStatus.RideAssigned $
            RideAssignedOS.RideAssignedOrder
              { id = booking.id.getId,
                state = RideAssignedOS.orderState,
                ..
              }
      }
buildOnStatusMessage (DStatus.RideStartedReq Common.DRideStartedReq {..}) = do
  let Common.BookingDetails {..} = bookingDetails
  let image = Nothing
  let arrivalTimeTagGroup = Common.mkArrivalTimeTagGroup ride.driverArrivalTime
  fulfillment <- Common.mkFulfillment (Just driver) (Just driverStats) ride booking (Just vehicle) image (Just $ Tags.TG arrivalTimeTagGroup) Nothing False False False 0 booking.isSafetyPlus
  return $
    OnStatus.OnStatusMessage
      { order =
          OnStatus.RideStarted $
            RideStartedOS.RideStartedOrder
              { id = booking.id.getId,
                state = RideStartedOS.orderState,
                ..
              }
      }
buildOnStatusMessage (DStatus.RideCompletedReq Common.DRideCompletedReq {..}) = do
  let Common.BookingDetails {..} = bookingDetails
  let image = Nothing
  let arrivalTimeTagGroup = Common.mkArrivalTimeTagGroup ride.driverArrivalTime
  distanceTagGroup <- Common.buildDistanceTagGroup ride
  fulfillment <- Common.mkFulfillment (Just driver) (Just driverStats) ride booking (Just vehicle) image (Just $ Tags.TG (distanceTagGroup <> arrivalTimeTagGroup)) Nothing False False False 0 booking.isSafetyPlus
  quote <- Common.buildRideCompletedQuote ride fareParams
  return $
    OnStatus.OnStatusMessage
      { order =
          OnStatus.RideCompleted
            RideCompletedOS.RideCompletedOrder
              { id = booking.id.getId,
                state = RideCompletedOS.orderState,
                quote,
                payment = Just $ Common.mkRideCompletedPayment ride.currency paymentMethodInfo paymentUrl,
                fulfillment = fulfillment
              }
      }
buildOnStatusMessage (DStatus.BookingCancelledReq Common.DBookingCancelledReq {..}) = do
  fulfillment <- forM bookingDetails $ \bookingDetails' -> do
    let Common.BookingDetails {driver, driverStats, vehicle, ride} = bookingDetails'
    let image = Nothing
    let arrivalTimeTagGroup = Common.mkArrivalTimeTagGroup ride.driverArrivalTime
    Common.mkFulfillment (Just driver) (Just driverStats) ride booking (Just vehicle) image (Just $ Tags.TG arrivalTimeTagGroup) Nothing False False False 0 booking.isSafetyPlus
  pure
    OnStatus.OnStatusMessage
      { order =
          OnStatus.BookingCancelled $
            BookingCancelledOS.BookingCancelledOrder
              { id = booking.id.getId,
                state = BookingCancelledOS.orderState,
                cancellation_reason = Common.castCancellationSource cancellationSource,
                fulfillment
              }
      }
buildOnStatusMessage (DStatus.BookingReallocationBuildReq DStatus.DBookingReallocationBuildReq {bookingReallocationInfo, bookingDetails}) = do
  let DStatus.BookingCancelledInfo {cancellationSource} = bookingReallocationInfo
  let Common.BookingDetails {..} = bookingDetails
  let image = Nothing
  let arrivalTimeTagGroup = Common.mkArrivalTimeTagGroup ride.driverArrivalTime
  fulfillment <- Common.mkFulfillment (Just driver) (Just driverStats) ride booking (Just vehicle) image (Just $ Tags.TG arrivalTimeTagGroup) Nothing False False False 0 booking.isSafetyPlus
  pure
    OnStatus.OnStatusMessage
      { order =
          OnStatus.BookingReallocation $
            BookingReallocationOS.BookingReallocationOrder
              { id = booking.id.getId,
                state = BookingReallocationOS.orderState, -----
                reallocation_reason = Common.castCancellationSource cancellationSource,
                fulfillment
              }
      }

buildOnStatusReqV2 ::
  ( MonadFlow m,
    EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CacheFlow m r
  ) =>
  DM.Merchant ->
  DRB.Booking ->
  DStatus.OnStatusBuildReq ->
  Maybe Text ->
  m Spec.OnStatusReq
buildOnStatusReqV2 merchant booking req mbMessageId = do
  msgId <- maybe generateGUID pure mbMessageId
  let bppId = getShortId $ merchant.subscriberId
      city = fromMaybe merchant.city booking.bapCity
      country = fromMaybe merchant.country booking.bapCountry
  bppUri <- BUtils.mkBppUri merchant.id.getId
  farePolicy <- SFP.getFarePolicyByEstOrQuoteIdWithoutFallback booking.quoteId
  bppConfig <- QBC.findByMerchantIdDomainAndVehicle booking.providerId "MOBILITY" (Utils.mapServiceTierToCategory booking.vehicleServiceTier) >>= fromMaybeM (InternalError "Beckn Config not found")
  buildOnStatusReqV2' Context.ON_STATUS Context.MOBILITY msgId bppId bppUri city country booking req farePolicy bppConfig

buildOnStatusReqV2' ::
  (MonadFlow m, EncFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  Context.Action ->
  Context.Domain ->
  Text ->
  Text ->
  BaseUrl ->
  Context.City ->
  Context.Country ->
  DRB.Booking ->
  DStatus.OnStatusBuildReq ->
  Maybe FarePolicyD.FullFarePolicy ->
  DBC.BecknConfig ->
  m Spec.OnStatusReq
buildOnStatusReqV2' action domain messageId bppSubscriberId bppUri city country booking req mbFarePolicy bppConfig = do
  ttl <- bppConfig.onStatusTTLSec & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
  bapUri <- parseBaseUrl booking.bapUri
  context <- CU.buildContextV2 action domain messageId (Just booking.transactionId) booking.bapId bapUri (Just bppSubscriberId) (Just bppUri) city country (Just ttl)
  message <- mkOnStatusMessageV2 req mbFarePolicy bppConfig
  pure $
    Spec.OnStatusReq
      { onStatusReqError = Nothing,
        onStatusReqContext = context,
        onStatusReqMessage = message
      }

mkOnStatusMessageV2 ::
  (MonadFlow m, EncFlow m r) =>
  DStatus.OnStatusBuildReq ->
  Maybe FarePolicyD.FullFarePolicy ->
  DBC.BecknConfig ->
  m (Maybe Spec.ConfirmReqMessage)
mkOnStatusMessageV2 res mbFarePolicy bppConfig = do
  order <- tfOrder res mbFarePolicy bppConfig
  pure . Just $
    Spec.ConfirmReqMessage
      { confirmReqMessageOrder = order
      }

tfOrder :: (MonadFlow m, EncFlow m r) => DStatus.OnStatusBuildReq -> Maybe FarePolicyD.FullFarePolicy -> DBC.BecknConfig -> m Spec.Order
tfOrder (DStatus.NewBookingBuildReq DNewBookingBuildReq {bookingId}) _ becknConfig =
  pure
    Spec.Order
      { orderId = Just bookingId.getId,
        orderStatus = Just $ show NewBookingOS.orderState, -- TODO::Beckn, confirm mapping as we only have 5 states in v2 spec.
        orderFulfillments = Nothing,
        orderBilling = Nothing,
        orderCancellation = Nothing,
        orderCancellationTerms = Just $ Utils.tfCancellationTerms Nothing Nothing,
        orderItems = Nothing,
        orderPayments = Nothing,
        orderProvider = Utils.tfProvider becknConfig,
        orderQuote = Nothing,
        orderCreatedAt = Nothing,
        orderUpdatedAt = Nothing --------To do keep booking created and updated time
      }
tfOrder (DStatus.RideAssignedReq req) mbFarePolicy becknConfig = Common.tfAssignedReqToOrder req mbFarePolicy becknConfig EventEnum.RIDE_ASSIGNED
tfOrder (DStatus.RideStartedReq req) mbFarePolicy becknConfig = Common.tfStartReqToOrder req mbFarePolicy becknConfig
tfOrder (DStatus.RideCompletedReq req) mbFarePolicy becknConfig = Common.tfCompleteReqToOrder req mbFarePolicy becknConfig
tfOrder (DStatus.BookingCancelledReq req) _ becknConfig = Common.tfCancelReqToOrder req becknConfig
tfOrder (DStatus.BookingReallocationBuildReq DBookingReallocationBuildReq {bookingReallocationInfo, bookingDetails}) _ becknConfig = do
  let DStatus.BookingCancelledInfo {cancellationSource} = bookingReallocationInfo
  let Common.BookingDetails {driver, driverStats, vehicle, booking, ride, isValueAddNP} = bookingDetails
  let image = Nothing
  let arrivalTimeTagGroup = Utils.mkArrivalTimeTagGroupV2 ride.driverArrivalTime
  fulfillment <- Utils.mkFulfillmentV2 (Just driver) (Just driverStats) ride booking (Just vehicle) image arrivalTimeTagGroup Nothing False False Nothing Nothing isValueAddNP Nothing False 0
  pure
    Spec.Order
      { orderId = Just $ booking.id.getId,
        orderStatus = Just $ show BookingReallocationOS.orderState, -- TODO::Beckn, confirm mapping as we only have 5 states in v2 spec.
        orderFulfillments = Just [fulfillment],
        orderCancellation =
          Just $
            Spec.Cancellation
              { cancellationCancelledBy = Just . show $ UtilsOU.castCancellationSource cancellationSource,
                cancellationReasonDescriptor = Nothing
              },
        orderBilling = Nothing,
        orderCancellationTerms = Just $ Utils.tfCancellationTerms Nothing Nothing,
        orderItems = Nothing,
        orderPayments = Utils.tfPayments booking bookingDetails.merchant becknConfig,
        orderProvider = Utils.tfProvider becknConfig,
        orderQuote = Nothing,
        orderCreatedAt = Just booking.createdAt,
        orderUpdatedAt = Just booking.updatedAt
      }
