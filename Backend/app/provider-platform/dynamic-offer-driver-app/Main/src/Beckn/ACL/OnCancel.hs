{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnCancel
  ( buildOnCancelMessageV2,
    module Reexport,
  )
where

import qualified Beckn.ACL.Common as Common
import qualified Beckn.OnDemand.Utils.Common as BUtils
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import BecknV2.OnDemand.Utils.Constructors
import qualified BecknV2.OnDemand.Utils.Context as CU
import BecknV2.OnDemand.Utils.Payment
import qualified Data.List as L
import qualified Domain.Action.UI.Person as DP
import Domain.Types
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Location as DLoc
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.FarePolicy as FarePolicyD
import qualified Domain.Types.Merchant as DM
import Domain.Types.OnCancel as Reexport
import qualified Domain.Types.OnCancel as OC
import Domain.Types.Ride
import qualified Domain.Types.Vehicle as DVeh
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.External.Encryption (decrypt)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import qualified Kernel.Types.Beckn.Gps as Gps
import Kernel.Types.Error
import Kernel.Types.Id
import qualified Kernel.Types.Price
import Kernel.Utils.Common hiding (mkPrice)
import qualified Kernel.Utils.Common as Common (mkPrice)
import SharedLogic.FareCalculator
import qualified SharedLogic.FarePolicy as SFP
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRiderDetails
import qualified Storage.Queries.SearchRequest as QSearchReq
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Error

buildOnCancelMessageV2 ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  DM.Merchant ->
  Maybe Context.City ->
  Maybe Context.Country ->
  Text ->
  OnCancelBuildReq ->
  Maybe Text ->
  Maybe FarePolicyD.FullFarePolicy ->
  m Spec.OnCancelReq
buildOnCancelMessageV2 merchant mbBapCity mbBapCountry cancelStatus (OC.BookingCancelledBuildReqV2 OC.DBookingCancelledReqV2 {..}) mbMsgId mbPreFetchedFarePolicy = do
  msgId <- maybe generateGUID return mbMsgId
  let bppId = getShortId $ merchant.subscriberId
      city = fromMaybe merchant.city mbBapCity
      country = fromMaybe merchant.country mbBapCountry
  bppUri <- BUtils.mkBppUri merchant.id.getId
  riderId <- booking.riderId & fromMaybeM (BookingFieldNotPresent "rider_id")
  mbRide <- QRide.findOneByBookingId booking.id
  riderDetails <- runInReplica $ QRiderDetails.findById riderId >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
  customerPhoneNo <- decrypt riderDetails.mobileNumber
  let vehicleCategory = Utils.mapServiceTierToCategory booking.vehicleServiceTier
  mbVehicle <- maybe (pure Nothing) (runInReplica . QVeh.findById . (.driverId)) mbRide
  mbPerson <- maybe (pure Nothing) (runInReplica . QPerson.findById . (.driverId)) mbRide
  finalFarePolicy <- case mbPreFetchedFarePolicy of
    Just _ -> pure mbPreFetchedFarePolicy
    Nothing -> SFP.getFarePolicyByEstOrQuoteIdWithoutFallback booking.quoteId
  becknConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id (show Context.MOBILITY) vehicleCategory >>= fromMaybeM (InternalError "Beckn Config not found")
  let driverName = DP.getPersonFullName =<< mbPerson
  driverPhone <- maybe (pure Nothing) DP.getPersonNumber mbPerson
  mbSearchReq <- runInReplica $ QSearchReq.findByTransactionIdAndMerchantId booking.transactionId booking.providerId
  let mbPolyline = mbSearchReq >>= (.encodedPolyline)
      mbRideOtp = mbRide <&> (.otp)
  buildOnCancelReq Context.ON_CANCEL Context.MOBILITY msgId bppId bppUri city country cancelStatus merchant driverName customerPhoneNo (OC.BookingCancelledBuildReqV2 OC.DBookingCancelledReqV2 {..}) (mbRide <&> (.status)) becknConfig mbVehicle finalFarePolicy driverPhone mbPolyline mbRideOtp

buildOnCancelReq ::
  (MonadFlow m, EncFlow m r, CacheFlow m r) =>
  Context.Action ->
  Context.Domain ->
  Text ->
  Text ->
  BaseUrl ->
  Context.City ->
  Context.Country ->
  Text ->
  DM.Merchant ->
  Maybe Text ->
  Text ->
  OC.OnCancelBuildReq ->
  Maybe RideStatus ->
  DBC.BecknConfig ->
  Maybe DVeh.Vehicle ->
  Maybe FarePolicyD.FullFarePolicy ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  m Spec.OnCancelReq
buildOnCancelReq action domain messageId bppSubscriberId bppUri city country cancelStatus merchant driverName customerPhoneNo (OC.BookingCancelledBuildReqV2 OC.DBookingCancelledReqV2 {..}) rideStatus becknConfig mbVehicle mbFarePolicy driverPhone mbPolyline mbRideOtp = do
  ttl <- becknConfig.onCancelTTLSec & fromMaybeM (InternalError "Invalid ttl") <&> Utils.computeTtlISO8601
  bapUri <- Kernel.Prelude.parseBaseUrl booking.bapUri
  context <- CU.buildContextV2_1 action domain messageId (Just booking.transactionId) booking.bapId bapUri (Just bppSubscriberId) (Just bppUri) city country (Just ttl)
  message <- buildOnCancelMessageReqV2 booking cancelStatus cancellationSource cancellationFee merchant driverName customerPhoneNo becknConfig rideStatus mbVehicle mbFarePolicy driverPhone mbPolyline mbRideOtp
  pure $
    Spec.OnCancelReq
      { onCancelReqError = Nothing,
        onCancelReqContext = context,
        onCancelReqMessage = message
      }

buildOnCancelMessageReqV2 :: (CacheFlow m r) => DRB.Booking -> Text -> SBCR.CancellationSource -> Maybe PriceAPIEntity -> DM.Merchant -> Maybe Text -> Text -> DBC.BecknConfig -> Maybe RideStatus -> Maybe DVeh.Vehicle -> Maybe FarePolicyD.FullFarePolicy -> Maybe Text -> Maybe Text -> Maybe Text -> m (Maybe Spec.ConfirmReqMessage)
buildOnCancelMessageReqV2 booking cancelStatus cancellationSource cancellationFee merchant driverName customerPhoneNo becknConfig rideStatus mbVehicle mbFarePolicy driverPhone mbPolyline mbRideOtp = do
  order <- tfOrder booking cancelStatus cancellationSource cancellationFee merchant driverName customerPhoneNo becknConfig rideStatus mbVehicle mbFarePolicy driverPhone mbPolyline mbRideOtp
  pure . Just $
    Spec.ConfirmReqMessage
      { confirmReqMessageOrder = order
      }

tfOrder :: (CacheFlow m r) => DRB.Booking -> Text -> SBCR.CancellationSource -> Maybe PriceAPIEntity -> DM.Merchant -> Maybe Text -> Text -> DBC.BecknConfig -> Maybe RideStatus -> Maybe DVeh.Vehicle -> Maybe FarePolicyD.FullFarePolicy -> Maybe Text -> Maybe Text -> Maybe Text -> m Spec.Order
tfOrder booking cancelStatus cancellationSource cancellationFee merchant driverName customerPhoneNo becknConfig rideStatus mbVehicle mbFarePolicy driverPhone mbPolyline mbRideOtp = do
  mbCachedBapTerms <- BUtils.getCachedBapTerms booking.transactionId
  pure $
    Spec.Order
      { orderId = Just booking.id.getId,
        orderStatus = Just cancelStatus,
        orderFulfillments = tfFulfillments booking driverName customerPhoneNo rideStatus mbVehicle driverPhone mbPolyline mbRideOtp,
        orderCancellation = tfCancellation cancellationSource,
        orderBilling = Nothing,
        orderCancellationTerms = Just $ tfCancellationTerms cancellationFee,
        orderItems = tfItems booking merchant mbFarePolicy,
        orderPayments = tfPayments (Common.mkPrice (Just booking.currency) booking.estimatedFare) merchant becknConfig booking.paymentId,
        orderProvider = BUtils.tfProvider becknConfig,
        orderQuote = tfQuotation booking,
        orderTags = BUtils.mkOrderTagsWithBapTerms mbCachedBapTerms (Just 0) becknConfig,
        orderCreatedAt = Just booking.createdAt,
        orderUpdatedAt = Just booking.updatedAt
      }

tfFulfillments :: DRB.Booking -> Maybe Text -> Text -> Maybe RideStatus -> Maybe DVeh.Vehicle -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe [Spec.Fulfillment]
tfFulfillments booking driverName customerPhoneNo rideStatus mbVehicle driverPhone mbPolyline mbRideOtp = do
  let otp = mbRideOtp <|> booking.specialZoneOtpCode
      stops = mkCancelStops booking.fromLocation booking.toLocation otp
  Just
    [ emptyFulfillment
        { Spec.fulfillmentId = Just $ BUtils.getBookingFulfillmentId booking,
          Spec.fulfillmentState = mkFulfillmentState rideStatus,
          Spec.fulfillmentStops = stops,
          Spec.fulfillmentType = Just $ Utils.tripCategoryToFulfillmentType booking.tripCategory,
          Spec.fulfillmentAgent = tfAgent booking driverName driverPhone,
          Spec.fulfillmentCustomer = tfCustomer booking customerPhoneNo,
          Spec.fulfillmentVehicle = tfVehicle mbVehicle,
          Spec.fulfillmentTags = BUtils.mkRouteInfoTagsFromPolyline mbPolyline
        }
    ]
  where
    mkFulfillmentState rideStatus' =
      Just $
        Spec.FulfillmentState
          { fulfillmentStateDescriptor =
              Just $ emptyDescriptor {Spec.descriptorCode = (Just . show . BUtils.mapRideStatus) rideStatus'}
          }

mkCancelStops :: DLoc.Location -> Maybe DLoc.Location -> Maybe Text -> Maybe [Spec.Stop]
mkCancelStops origin _mbDestination mbOtp =
  let originGps = Utils.gpsToText $ Gps.Gps {lat = origin.lat, lon = origin.lon}
   in Just
        [ emptyStop
            { Spec.stopLocation =
                Just $ emptyLocation {Spec.locationGps = originGps},
              Spec.stopType = Just "START",
              Spec.stopAuthorization = mbOtp >>= \otpVal ->
                Just $
                  Spec.Authorization
                    { authorizationToken = Just otpVal,
                      authorizationType = Just "OTP",
                      authorizationStatus = Nothing,
                      authorizationValidTo = Nothing
                    }
            }
        ]

tfQuotation :: DRB.Booking -> Maybe Spec.Quotation
tfQuotation booking =
  let rawBreakups = mkFareParamsBreakups (\x -> x) (\title money -> (title, money)) booking.fareParams
      normalized = mapMaybe (\(title, money) -> (,money) <$> BUtils.normalizeBreakupTitle title) rawBreakups
      aggregated = BUtils.aggregateBreakupsWithTotal (Just booking.estimatedFare) booking.currency normalized
      refundValue = Kernel.Types.Price.showPriceWithRoundingWithoutCurrency $ Kernel.Types.Price.mkPrice (Just booking.currency) booking.estimatedFare
      cancellationBreakup = mkBreakup "CANCELLATION_CHARGES" "0"
      refundBreakup = mkBreakup "REFUND" ("-" <> refundValue)
   in Just
        emptyQuotation
          { Spec.quotationBreakup = Just $ aggregated <> [cancellationBreakup, refundBreakup],
            Spec.quotationPrice = Just emptyPrice
              { Spec.priceCurrency = Just $ show booking.currency,
                Spec.priceValue = Just "0"
              }
          }
  where
    mkBreakup title value =
      Spec.QuotationBreakupInner
        { quotationBreakupInnerTitle = Just title,
          quotationBreakupInnerPrice = Just emptyPrice
            { Spec.priceCurrency = Just $ show booking.currency,
              Spec.priceValue = Just value
            }
        }

tfPayments :: Price -> DM.Merchant -> DBC.BecknConfig -> Maybe Text -> Maybe [Spec.Payment]
tfPayments estimatedFare merchant bppConfig mbPaymentId = do
  let mPrice = Just estimatedFare
  let mkParams :: Maybe BknPaymentParams = decodeFromText =<< bppConfig.paymentParamsJson
  Just . L.singleton $ mkPayment (show merchant.city) (show bppConfig.collectedBy) Enums.NOT_PAID mPrice mbPaymentId mkParams bppConfig.settlementType bppConfig.settlementWindow bppConfig.staticTermsUrl bppConfig.buyerFinderFee False Nothing Nothing

tfItems :: DRB.Booking -> DM.Merchant -> Maybe FarePolicyD.FullFarePolicy -> Maybe [Spec.Item]
tfItems booking merchant mbFarePolicy =
  let farePolicyTags = maybeToList (BUtils.mkSpecFarePolicyTagsFromPolicy (FarePolicyD.fullFarePolicyToFarePolicy <$> mbFarePolicy))
      infoTags = fromMaybe [] (BUtils.mkInfoTagGroup booking.distanceToPickup booking.dqDurationToPickup)
      featureList = BUtils.mkFeatureListTags booking.isAirConditioned
      combinedTags = Just $ farePolicyTags <> infoTags <> [featureList]
   in Just
        [ emptyItem
            { Spec.itemDescriptor = BUtils.tfItemDescriptor booking,
              Spec.itemFulfillmentIds = Just [BUtils.getBookingFulfillmentId booking],
              Spec.itemId = Just $ maybe (Common.mkItemId merchant.shortId.getShortId booking.vehicleServiceTier) getId (booking.estimateId),
              Spec.itemCategoryIds = Just [BUtils.tripCategoryToCategoryCode booking.tripCategory],
              Spec.itemPaymentIds = tfPaymentId booking.paymentId,
              Spec.itemPrice = BUtils.tfItemPrice booking.estimatedFare booking.currency,
              Spec.itemTags = combinedTags
            }
        ]

tfPaymentId :: Maybe Text -> Maybe [Text]
tfPaymentId mbPaymentId = do
  paymentId <- mbPaymentId
  Just [paymentId]


tfVehicle :: Maybe DVeh.Vehicle -> Maybe Spec.Vehicle
tfVehicle mbVehicle =
  mbVehicle >>= \vehicle -> do
    let (category, variant) = BUtils.castVariant vehicle.variant
    Just $
      emptyVehicle
        { Spec.vehicleColor = Just vehicle.color,
          Spec.vehicleModel = Just vehicle.model,
          Spec.vehicleRegistration = Just vehicle.registrationNo,
          Spec.vehicleCategory = Just category,
          Spec.vehicleVariant = Just variant,
          Spec.vehicleCapacity = vehicle.capacity
        }

tfAgent :: DRB.Booking -> Maybe Text -> Maybe Text -> Maybe Spec.Agent
tfAgent _booking driverName driverPhone = do
  Just $
    Spec.Agent
      { agentContact = Common.tfContact driverPhone,
        agentPerson = Just $ emptyPerson {Spec.personName = driverName}
      }

tfCustomer :: DRB.Booking -> Text -> Maybe Spec.Customer
tfCustomer booking customerPhoneNo = do
  Just $
    Spec.Customer
      { customerContact =
          Just Spec.Contact {contactPhone = Just customerPhoneNo},
        customerPerson =
          Just $ emptyPerson {Spec.personName = booking.riderName}
      }

tfCancellation :: SBCR.CancellationSource -> Maybe Spec.Cancellation
tfCancellation cancellationSource =
  Just $
    Spec.Cancellation
      { cancellationCancelledBy = castCancellatonSource cancellationSource,
        cancellationReason = Just $ Spec.CancellationReason
          { cancellationReasonDescriptor = Just $ emptyDescriptor {Spec.descriptorCode = Just $ castReasonCode cancellationSource}
          }
      }
  where
    castCancellatonSource = \case
      SBCR.ByUser -> Just (show Enums.CONSUMER)
      SBCR.ByDriver -> Just (show Enums.PROVIDER)
      _ -> Just (show Enums.PROVIDER)
    castReasonCode = \case
      SBCR.ByDriver -> show Enums.RIDE_ACCEPTED_MISTAKENLY -- 013
      SBCR.ByAllocator -> show Enums.NO_DRIVERS_AVAILABLE -- 011
      _ -> show Enums.RIDE_ACCEPTED_MISTAKENLY -- default for provider cancellation

tfCancellationTerms :: Maybe PriceAPIEntity -> [Spec.CancellationTerm]
tfCancellationTerms _cancellationFee = BUtils.tfCancellationTerms Nothing Nothing
