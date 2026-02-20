{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedLabels #-}

module Beckn.ACL.Common where

import qualified Beckn.OnDemand.Utils.Common as Utils
import qualified Beckn.Types.Core.Taxi.Common.CancellationSource as Common
import qualified Beckn.Types.Core.Taxi.Common.Payment as Payment
import qualified Beckn.Types.Core.Taxi.Search as Search
import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Utils.Common as Utils
import Control.Lens ((^?), _Just, _head)
import Data.Generics.Labels ()
import qualified Data.Text as T
import Domain.Action.Beckn.Common as Common
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import Kernel.External.Maps.Types as Maps
import qualified Kernel.External.Payment.Interface as EPayment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Beckn.DecimalValue as DecimalValue
import Kernel.Types.Confidence
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Servant.Client.Core as SCC
import SharedLogic.Search as SLS
import Tools.Error

validatePrices :: (MonadThrow m, Log m, Num a, Ord a) => a -> a -> m ()
validatePrices price priceWithDiscount = do
  when (price < 0) $ throwError $ InvalidRequest "price is less than zero"
  when (priceWithDiscount < 0) $ throwError $ InvalidRequest "discounted price is less than zero"
  when (priceWithDiscount > price) $ throwError $ InvalidRequest "price is lesser than discounted price"

castDPaymentCollector :: DMPM.PaymentCollector -> Payment.PaymentCollector
castDPaymentCollector DMPM.BAP = Payment.BAP
castDPaymentCollector DMPM.BPP = Payment.BPP

castDPaymentType :: DMPM.PaymentType -> Payment.PaymentType
castDPaymentType DMPM.ON_FULFILLMENT = Payment.ON_FULFILLMENT
castDPaymentType DMPM.POSTPAID = Payment.ON_FULFILLMENT

castDPaymentInstrument :: DMPM.PaymentInstrument -> Payment.PaymentInstrument
castDPaymentInstrument (DMPM.Card DMPM.DefaultCardType) = Payment.Card Payment.DefaultCardType
castDPaymentInstrument (DMPM.Wallet DMPM.DefaultWalletType) = Payment.Wallet Payment.DefaultWalletType
castDPaymentInstrument DMPM.UPI = Payment.UPI
castDPaymentInstrument DMPM.NetBanking = Payment.NetBanking
castDPaymentInstrument DMPM.Cash = Payment.Cash
castDPaymentInstrument DMPM.BoothOnline = Payment.BoothOnline

castPaymentCollector :: Payment.PaymentCollector -> DMPM.PaymentCollector
castPaymentCollector Payment.BAP = DMPM.BAP
castPaymentCollector Payment.BPP = DMPM.BPP

castPaymentType :: Payment.PaymentType -> DMPM.PaymentType
castPaymentType Payment.ON_FULFILLMENT = DMPM.ON_FULFILLMENT
castPaymentType Payment.POSTPAID = DMPM.ON_FULFILLMENT

castPaymentInstrument :: Payment.PaymentInstrument -> DMPM.PaymentInstrument
castPaymentInstrument (Payment.Card Payment.DefaultCardType) = DMPM.Card DMPM.DefaultCardType
castPaymentInstrument (Payment.Wallet Payment.DefaultWalletType) = DMPM.Wallet DMPM.DefaultWalletType
castPaymentInstrument Payment.UPI = DMPM.UPI
castPaymentInstrument Payment.NetBanking = DMPM.NetBanking
castPaymentInstrument Payment.Cash = DMPM.Cash
castPaymentInstrument Payment.BoothOnline = DMPM.BoothOnline

mkLocation :: SLS.SearchReqLocation -> Search.Location
mkLocation info =
  Search.Location
    { gps =
        Search.Gps
          { lat = info.gps.lat,
            lon = info.gps.lon
          },
      address =
        Just
          Search.Address
            { locality = info.address.area,
              state = info.address.state,
              country = info.address.country,
              building = info.address.building,
              street = info.address.street,
              city = info.address.city,
              area_code = info.address.areaCode,
              door = info.address.door,
              ward = info.address.ward
            }
    }

castCancellationSource :: Common.CancellationSource -> SBCR.CancellationSource
castCancellationSource = \case
  Common.ByUser -> SBCR.ByUser
  Common.ByDriver -> SBCR.ByDriver
  Common.ByMerchant -> SBCR.ByMerchant
  Common.ByAllocator -> SBCR.ByAllocator
  Common.ByApplication -> SBCR.ByApplication
  Common.ByFleetOwner -> SBCR.ByMerchant -- ByFleetOwner not available in rider-platform, mapping to ByMerchant

getTagV2' :: Tag.BecknTagGroup -> Tag.BecknTag -> Maybe [Spec.TagGroup] -> Maybe Text
getTagV2' tagGroupCode tagCode mbTagGroups =
  mbTagGroups >>= getTagV2 tagGroupCode tagCode

getTagV2 :: Tag.BecknTagGroup -> Tag.BecknTag -> [Spec.TagGroup] -> Maybe Text
getTagV2 tagGroupCode tagCode tagGroups = do
  tagGroup <- find (\tagGroup -> descriptorCode tagGroup.tagGroupDescriptor == Just (show tagGroupCode)) tagGroups
  tagGroupList <- tagGroup.tagGroupList
  tag <- find (\tag -> descriptorCode tag.tagDescriptor == Just (show tagCode)) tagGroupList
  tag.tagValue
  where
    descriptorCode :: Maybe Spec.Descriptor -> Maybe Text
    descriptorCode = (>>= (.descriptorCode))

parseBookingDetails :: (MonadFlow m, CacheFlow m r) => Spec.Order -> Text -> m Common.BookingDetails
parseBookingDetails order msgId = do
  bppBookingId <- order.orderId & fromMaybeM (InvalidRequest "order_id is not present in RideAssigned Event.")
  isInitiatedByCronJob <- (\(val :: Maybe Bool) -> isJust val) <$> Hedis.safeGet (makeContextMessageIdStatusSyncKey msgId)
  bppRideId <- order.orderFulfillments ^? _Just . _head . #fulfillmentId . _Just & fromMaybeM (InvalidRequest "fulfillment_id is not present in RideAssigned Event.")
  stops <- order.orderFulfillments ^? _Just . _head . #fulfillmentStops . _Just & fromMaybeM (InvalidRequest "fulfillment_stops is not present in RideAssigned Event.")
  start <- Utils.getStartLocation stops & fromMaybeM (InvalidRequest "pickup stop is not present in RideAssigned Event.")
  otp <- start.stopAuthorization >>= (.authorizationToken) & fromMaybeM (InvalidRequest "authorization_token is not present in RideAssigned Event.")
  driverName <- order.orderFulfillments ^? _Just . _head . #fulfillmentAgent . _Just . #agentPerson . _Just . #personName . _Just & fromMaybeM (InvalidRequest "driverName is not present in RideAssigned Event.")
  driverMobileNumber <- order.orderFulfillments ^? _Just . _head . #fulfillmentAgent . _Just . #agentContact . _Just . #contactPhone . _Just & fromMaybeM (InvalidRequest "driverMobileNumber is not present in RideAssigned Event.")
  let tagGroups = order.orderFulfillments ^? _Just . _head . #fulfillmentAgent . _Just . #agentPerson . _Just . #personTags . _Just
  let rating :: Maybe HighPrecMeters = readMaybe . T.unpack =<< getTagV2' Tag.DRIVER_DETAILS Tag.RATING tagGroups
      registeredAt :: Maybe UTCTime = readMaybe . T.unpack =<< getTagV2' Tag.DRIVER_DETAILS Tag.REGISTERED_AT tagGroups
  let driverImage = order.orderFulfillments ^? _Just . _head . #fulfillmentAgent . _Just . #agentPerson . _Just . #personImage . _Just . #imageUrl . _Just
  let vehicleColor = order.orderFulfillments ^? _Just . _head . #fulfillmentVehicle . _Just . #vehicleColor . _Just
      vehicleModel = order.orderFulfillments ^? _Just . _head . #fulfillmentVehicle . _Just . #vehicleModel . _Just
      vehicleNumber = order.orderFulfillments ^? _Just . _head . #fulfillmentVehicle . _Just . #vehicleRegistration . _Just
  pure $
    Common.BookingDetails
      { bppBookingId = Id bppBookingId,
        bppRideId = Id bppRideId,
        driverMobileCountryCode = Just "+91", -----------TODO needs to be added in agent Tags------------
        driverRating = realToFrac <$> rating,
        driverRegisteredAt = registeredAt,
        vehicleModel = fromMaybe "UNKWOWN" vehicleModel,
        vehicleNumber = fromMaybe "UNKWOWN" vehicleNumber,
        driverAlternatePhoneNumber = Nothing,
        ..
      }

parseRideAssignedEvent :: (MonadFlow m, CacheFlow m r) => Spec.Order -> Text -> Text -> m Common.RideAssignedReq
parseRideAssignedEvent order msgId txnId = do
  let tagGroups = order.orderFulfillments ^? _Just . _head . #fulfillmentAgent . _Just . #agentPerson . _Just . #personTags . _Just
  let tagGroupsFullfillment = order.orderFulfillments ^? _Just . _head . #fulfillmentTags . _Just
  let isDriverBirthDay = isJust $ getTagV2' Tag.DRIVER_DETAILS Tag.IS_DRIVER_BIRTHDAY tagGroups
      isFreeRide = isJust $ getTagV2' Tag.DRIVER_DETAILS Tag.IS_FREE_RIDE tagGroups
      vehicleAge :: Maybe Months = readMaybe . T.unpack =<< getTagV2' Tag.VEHICLE_AGE_INFO Tag.VEHICLE_AGE tagGroupsFullfillment
      driverAlternateNumber :: Maybe Text = getTagV2' Tag.DRIVER_DETAILS Tag.DRIVER_ALTERNATE_NUMBER tagGroups
      (driverAccountId :: Maybe EPayment.AccountId) = getTagV2' Tag.DRIVER_DETAILS Tag.DRIVER_ACCOUNT_ID tagGroups
      driverTrackingUrl :: Maybe BaseUrl = SCC.parseBaseUrl . T.unpack =<< getTagV2' Tag.DRIVER_DETAILS Tag.DRIVER_TRACKING_URL tagGroups
      previousRideEndPos = getLocationFromTagV2 tagGroupsFullfillment Tag.FORWARD_BATCHING_REQUEST_INFO Tag.PREVIOUS_RIDE_DROP_LOCATION_LAT Tag.PREVIOUS_RIDE_DROP_LOCATION_LON
      isAlreadyFav = isJust $ getTagV2' Tag.DRIVER_DETAILS Tag.IS_ALREADY_FAVOURITE tagGroups
      isSafetyPlus = isJust $ getTagV2' Tag.DRIVER_DETAILS Tag.IS_SAFETY_PLUS tagGroups
      favCount :: Maybe Int = readMaybe . T.unpack =<< getTagV2' Tag.DRIVER_DETAILS Tag.FAVOURITE_COUNT tagGroups
  let mbFareBreakupsQuotationBreakup = order.orderQuote >>= (.quotationBreakup)
  let fareBreakups = mbFareBreakupsQuotationBreakup <&> (mapMaybe mkDFareBreakup)
  bookingDetails <- parseBookingDetails order msgId
  return
    Common.RideAssignedReq
      { bookingDetails = bookingDetails{driverAlternatePhoneNumber = driverAlternateNumber},
        transactionId = txnId,
        ..
      }

parseRideStartedEvent :: (MonadFlow m, CacheFlow m r) => Spec.Order -> Text -> Text -> m Common.RideStartedReq
parseRideStartedEvent order msgId txnId = do
  bookingDetails <- parseBookingDetails order msgId
  stops <- order.orderFulfillments ^? _Just . _head . #fulfillmentStops . _Just & fromMaybeM (InvalidRequest "fulfillment_stops is not present in RideStarted Event.")
  start <- Utils.getStartLocation stops & fromMaybeM (InvalidRequest "pickup stop is not present in RideStarted Event.")
  let rideStartTime = start.stopTime >>= (.timeTimestamp)
      personTagsGroup = order.orderFulfillments ^? _Just . _head . #fulfillmentAgent . _Just . #agentPerson . _Just . #personTags . _Just
      tagGroups = order.orderFulfillments ^? _Just . _head . #fulfillmentTags . _Just
      startOdometerReading = readMaybe . T.unpack =<< getTagV2' Tag.RIDE_ODOMETER_DETAILS Tag.START_ODOMETER_READING tagGroups
      tripStartLocation = getLocationFromTagV2 personTagsGroup Tag.CURRENT_LOCATION Tag.CURRENT_LOCATION_LAT Tag.CURRENT_LOCATION_LON
      driverArrivalTime :: Maybe UTCTime = readMaybe . T.unpack =<< getTagV2' Tag.DRIVER_ARRIVED_INFO Tag.ARRIVAL_TIME tagGroups
      estimatedEndTimeRangeStart :: Maybe UTCTime = readMaybe . T.unpack =<< getTagV2' Tag.ESTIMATED_END_TIME_RANGE Tag.ESTIMATED_END_TIME_RANGE_START tagGroups
      estimatedEndTimeRangeEnd :: Maybe UTCTime = readMaybe . T.unpack =<< getTagV2' Tag.ESTIMATED_END_TIME_RANGE Tag.ESTIMATED_END_TIME_RANGE_END tagGroups
  pure $
    Common.RideStartedReq
      { bookingDetails,
        transactionId = txnId,
        endOtp_ = Just bookingDetails.otp,
        startOdometerReading,
        ..
      }

getLocationFromTagV2 :: Maybe [Spec.TagGroup] -> Tag.BecknTagGroup -> Tag.BecknTag -> Tag.BecknTag -> Maybe Maps.LatLong
getLocationFromTagV2 tagGroup key latKey lonKey =
  let tripStartLat :: Maybe Double = readMaybe . T.unpack =<< getTagV2' key latKey tagGroup
      tripStartLon :: Maybe Double = readMaybe . T.unpack =<< getTagV2' key lonKey tagGroup
   in Maps.LatLong <$> tripStartLat <*> tripStartLon

parseDriverArrivedEvent :: (MonadFlow m, CacheFlow m r) => Spec.Order -> Text -> Text -> m Common.DriverArrivedReq
parseDriverArrivedEvent order msgId txnId = do
  bookingDetails <- parseBookingDetails order msgId
  let tagGroups = order.orderFulfillments ^? _Just . _head . #fulfillmentTags . _Just
      arrivalTime = readMaybe . T.unpack =<< getTagV2' Tag.DRIVER_ARRIVED_INFO Tag.ARRIVAL_TIME tagGroups
  return $
    Common.DriverArrivedReq
      { bookingDetails,
        arrivalTime,
        transactionId = txnId
      }

parseRideCompletedEvent :: (MonadFlow m, CacheFlow m r) => Spec.Order -> Text -> Text -> m Common.RideCompletedReq
parseRideCompletedEvent order msgId txnId = do
  bookingDetails <- parseBookingDetails order msgId
  currency :: Currency <- order.orderQuote >>= (.quotationPrice) >>= (.priceCurrency) >>= (readMaybe . T.unpack) & fromMaybeM (InvalidRequest "quote.price.currency is not present in RideCompleted Event.")
  fare' <- order.orderQuote >>= (.quotationPrice) >>= (.priceValue) >>= decodeFromText & fromMaybeM (InvalidRequest "quote.price.value is not present in RideCompleted Event.")
  let fare = DecimalValue.DecimalValue fare'
  let totalFare = fare
      tagGroups = order.orderFulfillments ^? _Just . _head . #fulfillmentTags . _Just
      chargeableDistance :: Maybe HighPrecMeters = readMaybe . T.unpack =<< getTagV2' Tag.RIDE_DISTANCE_DETAILS Tag.CHARGEABLE_DISTANCE tagGroups
      traveledDistance :: Maybe HighPrecMeters = readMaybe . T.unpack =<< getTagV2' Tag.RIDE_DISTANCE_DETAILS Tag.TRAVELED_DISTANCE tagGroups
      endOdometerReading = readMaybe . T.unpack =<< getTagV2' Tag.RIDE_DISTANCE_DETAILS Tag.END_ODOMETER_READING tagGroups
      tollConfidence :: Maybe Confidence = readMaybe . T.unpack =<< getTagV2' Tag.TOLL_CONFIDENCE_INFO Tag.TOLL_CONFIDENCE tagGroups
      isValidRide :: Maybe Bool = readMaybe . T.unpack =<< getTagV2' Tag.RIDE_DETAILS_INFO Tag.IS_VALID_RIDE tagGroups
  fareBreakupsQuotationBreakup <- order.orderQuote >>= (.quotationBreakup) & fromMaybeM (InvalidRequest "quote breakup is not present in RideCompleted Event.")
  let fareBreakups = mapMaybe mkDFareBreakup fareBreakupsQuotationBreakup
  let personTagsGroup = order.orderFulfillments ^? _Just . _head . #fulfillmentAgent . _Just . #agentPerson . _Just . #personTags . _Just
      tripEndLocation = getLocationFromTagV2 personTagsGroup Tag.CURRENT_LOCATION Tag.CURRENT_LOCATION_LAT Tag.CURRENT_LOCATION_LON
      rideEndTime = (order.orderFulfillments ^? _Just . _head . #fulfillmentStops . _Just) >>= Utils.getDropLocation >>= (.stopTime) >>= (.timeTimestamp)
      paymentStatus = (order.orderPayments ^? _Just . _head . #paymentStatus . _Just) >>= readMaybe . T.unpack
  pure $
    Common.RideCompletedReq
      { bookingDetails,
        transactionId = txnId,
        fare = Utils.decimalValueToPrice currency fare,
        totalFare = Utils.decimalValueToPrice currency totalFare,
        chargeableDistance,
        traveledDistance,
        fareBreakups,
        paymentUrl = Nothing,
        ..
      }

mkDFareBreakup :: Spec.QuotationBreakupInner -> Maybe Common.DFareBreakup
mkDFareBreakup breakup = do
  val' <- breakup.quotationBreakupInnerPrice >>= (.priceValue) >>= decodeFromText
  let val = DecimalValue.DecimalValue val'
  currency :: Currency <- breakup.quotationBreakupInnerPrice >>= (.priceCurrency) >>= readMaybe . T.unpack
  title <- breakup.quotationBreakupInnerTitle
  pure $
    Common.DFareBreakup
      { amount = Utils.decimalValueToPrice currency val,
        description = title
      }

parseBookingCancelledEvent :: (MonadFlow m, CacheFlow m r) => Spec.Order -> Text -> Text -> m Common.BookingCancelledReq
parseBookingCancelledEvent order msgId txnId = do
  bppBookingId <- order.orderId & fromMaybeM (InvalidRequest "order_id is not present in BookingCancelled Event.")
  bookingDetails <-
    traverse (\_ -> parseBookingDetails order msgId) order.orderFulfillments
  cancellationSource <- order.orderCancellation >>= (.cancellationCancelledBy) & fromMaybeM (InvalidRequest "cancellationSource is not present in BookingCancelled Event.")
  return $
    Common.BookingCancelledReq
      { bppBookingId = Id bppBookingId,
        transactionId = txnId,
        bookingDetails,
        cancellationSource = Utils.castCancellationSourceV2 cancellationSource
      }

makeContextMessageIdStatusSyncKey :: Text -> Text
makeContextMessageIdStatusSyncKey msgId = "SyncAPI:Ride:Cron:Status:MessageId" <> msgId
