{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Yudhishthira where

import qualified BecknV2.OnDemand.Enums
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.BookingStatus as DBookingStatus
import qualified Domain.Types.Common as DCommon
import Domain.Types.Client
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person (Person)
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Quote as DQuote
import qualified Domain.Types.RecentLocation as DRecentLocation
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.ServiceTierType as DServiceTierType
import qualified Domain.Types.Trip as DTrip
import qualified Domain.Types.TripTerms as DTripTerms
import qualified Domain.Types.Extra.Booking as DEBooking
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.Version
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types.Application as YA
import qualified Lib.Yudhishthira.Types.Common as YTC
import qualified Lib.Yudhishthira.TypesTH as YTH
import qualified SharedLogic.Type as SLT

data LoginTagData = LoginTagData
  { id :: Id Person,
    gender :: Maybe SP.Gender,
    clientSdkVersion :: Maybe Version,
    clientBundleVersion :: Maybe Version,
    clientReactNativeVersion :: Maybe Text,
    clientConfigVersion :: Maybe Version,
    clientDevice :: Maybe Device
  }
  deriving (Show, ToJSON, FromJSON, Generic)

data RideData = RideData
  { allowedEditLocationAttempts :: Maybe Int,
    allowedEditPickupLocationAttempts :: Maybe Int,
    backendAppVersion :: Maybe Text,
    backendConfigVersion :: Maybe Version,
    cancellationFeeIfCancelled :: Maybe HighPrecMoney,
    chargeableDistance :: Maybe Distance,
    clientBundleVersion :: Maybe Version,
    clientConfigVersion :: Maybe Version,
    clientDevice :: Maybe Device,
    clientId :: Maybe (Id Client),
    clientSdkVersion :: Maybe Version,
    createdAt :: UTCTime,
    destinationReachedAt :: Maybe UTCTime,
    distanceUnit :: DistanceUnit,
    driverArrivalTime :: Maybe UTCTime,
    driverImage :: Maybe Text,
    driverMobileCountryCode :: Maybe Text,
    driverMobileNumber :: Text,
    driverName :: Text,
    driverRating :: Maybe Centesimal,
    driverRegisteredAt :: Maybe UTCTime,
    endOdometerReading :: Maybe Centesimal,
    endOtp :: Maybe Text,
    fare :: Maybe Price,
    favCount :: Maybe Int,
    feedbackSkipped :: Bool,
    hasStops :: Maybe Bool,
    insuredAmount :: Maybe Text,
    isAlreadyFav :: Maybe Bool,
    isFreeRide :: Maybe Bool,
    isInsured :: Bool,
    isPetRide :: Bool,
    isSafetyPlus :: Bool,
    onlinePayment :: Bool,
    otp :: Text,
    pickupRouteCallCount :: Maybe Int,
    rideEndTime :: Maybe UTCTime,
    rideRating :: Maybe Int,
    rideStartTime :: Maybe UTCTime,
    safetyCheckStatus :: Maybe Bool,
    showDriversPreviousRideDropLoc :: Bool,
    startOdometerReading :: Maybe Centesimal,
    talkedWithDriver :: Maybe Bool,
    tipAmount :: Maybe Price,
    totalFare :: Maybe Price,
    trackingUrl :: Maybe BaseUrl,
    traveledDistance :: Maybe Distance,
    updatedAt :: UTCTime,
    vehicleColor :: Maybe Text,
    vehicleModel :: Text,
    vehicleNumber :: Text,
    wasRideSafe :: Maybe Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data CustomerData = CustomerData
  { gender :: SP.Gender,
    mobileNumber :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data EndRideOffersTagData = EndRideOffersTagData
  { customerData :: CustomerData,
    rideData :: RideData
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data SearchRequestData = SearchRequestData
  { id :: Text,
    startTime :: UTCTime,
    returnTime :: Maybe UTCTime,
    roundTrip :: Maybe Bool,
    validTill :: UTCTime,
    hasStops :: Maybe Bool,
    riderId :: Text,
    clientId :: Maybe Text,
    fromLocationId :: Text,
    toLocationId :: Maybe Text,
    stopsCount :: Int,
    distance :: Maybe Distance,
    maxDistance :: Maybe Distance,
    distanceUnit :: DistanceUnit,
    estimatedRideDuration :: Maybe Seconds,
    estimatedRideStaticDuration :: Maybe Seconds,
    device :: Maybe Text,
    merchantId :: Text,
    merchantOperatingCityId :: Text,
    disabilityTag :: Maybe Text,
    customerExtraFee :: Maybe Price,
    isPetRide :: Maybe Bool,
    autoAssignEnabled :: Maybe Bool,
    autoAssignEnabledV2 :: Maybe Bool,
    selectedPaymentMethodId :: Maybe Text,
    selectedPaymentInstrument :: Maybe Text,
    riderPreferredOption :: Text,
    createdAt :: UTCTime,
    clientBundleVersion :: Maybe Text,
    clientSdkVersion :: Maybe Text,
    clientConfigVersion :: Maybe Text,
    cloudType :: Maybe Text,
    clientReactNativeVersion :: Maybe Text,
    backendConfigVersion :: Maybe Text,
    backendAppVersion :: Maybe Text,
    totalRidesCount :: Maybe Int,
    isAdvanceBookingEnabled :: Maybe Bool,
    isMeterRideSearch :: Maybe Bool,
    isDashboardRequest :: Maybe Bool,
    placeNameSource :: Maybe Text,
    initiatedBy :: Maybe Text,
    onSearchFailed :: Maybe Bool,
    hasMultimodalSearch :: Maybe Bool,
    routeCode :: Maybe Text,
    originStopCode :: Maybe Text,
    destinationStopCode :: Maybe Text,
    vehicleCategory :: Maybe Text,
    allJourneysLoaded :: Maybe Bool,
    searchMode :: Maybe Text,
    isMultimodalSearch :: Maybe Bool,
    multimodalSearchRequestId :: Maybe Text,
    fromSpecialLocationId :: Maybe Text,
    toSpecialLocationId :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

-- Mirrors `DRB.Booking` for dynamic logic (e.g. OFFERS_FRAUD_CHECKS). Omits `bookingDetails`; replaces
-- `fromLocation` / `initialPickupLocation` with ids and `paymentMethodId` with `Text` (YTH.genDef).
data BookingData = BookingData
  { backendAppVersion :: Maybe Text,
    backendConfigVersion :: Maybe Version,
    billingCategory :: SLT.BillingCategory,
    bppBookingId :: Maybe (Id DRB.BPPBooking),
    bppEstimateId :: Text,
    clientBundleVersion :: Maybe Version,
    clientConfigVersion :: Maybe Version,
    clientDevice :: Maybe Device,
    clientId :: Maybe (Id Client),
    clientSdkVersion :: Maybe Version,
    commission :: Maybe HighPrecMoney,
    configInExperimentVersions :: [LYT.ConfigVersionMap],
    createdAt :: UTCTime,
    dashboardAgentId :: Maybe Text,
    disabilityTag :: Maybe Text,
    discount :: Maybe Price,
    displayBookingId :: Maybe Text,
    distanceUnit :: DistanceUnit,
    driverInsuredAmount :: Maybe Text,
    driverPreference :: Maybe [Text],
    estimatedDistance :: Maybe Distance,
    estimatedDuration :: Maybe Seconds,
    estimatedFare :: Price,
    estimatedStaticDuration :: Maybe Seconds,
    estimatedTotalFare :: Price,
    fromLocationId :: Text,
    fulfillmentId :: Maybe Text,
    hasStops :: Maybe Bool,
    id :: Id DRB.Booking,
    initialPickupLocationId :: Text,
    initiatedBy :: Maybe DTrip.TripParty,
    insuredAmount :: Maybe Text,
    isAirConditioned :: Maybe Bool,
    isBookingUpdated :: Bool,
    isDashboardRequest :: Maybe Bool,
    isInsured :: Bool,
    isMultimodalSearch :: Maybe Bool,
    isPetRide :: Bool,
    isReferredRide :: Maybe Bool,
    isScheduled :: Bool,
    merchantId :: Id DMerchant.Merchant,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    multimodalSearchRequestId :: Maybe Text,
    paymentInstrument :: Maybe DMPM.PaymentInstrument,
    paymentMethodId :: Maybe Text,
    paymentMode :: Maybe DMPM.PaymentMode,
    paymentStatus :: Maybe DEBooking.PaymentStatus,
    paymentUrl :: Maybe Text,
    preferSafetyPlus :: Bool,
    primaryExophone :: Text,
    providerId :: Text,
    providerUrl :: BaseUrl,
    quoteId :: Maybe (Id DQuote.Quote),
    recentLocationId :: Maybe (Id DRecentLocation.RecentLocation),
    requiresPaymentBeforeConfirm :: Bool,
    returnTime :: Maybe UTCTime,
    riderId :: Id SP.Person,
    roundTrip :: Maybe Bool,
    selectedOfferId :: Maybe Text,
    serviceTierName :: Maybe Text,
    serviceTierShortDesc :: Maybe Text,
    specialLocationName :: Maybe Text,
    specialLocationSupportNumber :: Maybe Text,
    specialLocationTag :: Maybe Text,
    startTime :: UTCTime,
    status :: DBookingStatus.BookingStatus,
    transactionId :: Text,
    tripCategory :: Maybe DCommon.TripCategory,
    tripTerms :: Maybe DTripTerms.TripTerms,
    updatedAt :: UTCTime,
    vehicleCategory :: Maybe BecknV2.OnDemand.Enums.VehicleCategory,
    vehicleIconUrl :: Maybe BaseUrl,
    vehicleServiceTierAirConditioned :: Maybe Double,
    vehicleServiceTierSeatingCapacity :: Maybe Int,
    vehicleServiceTierType :: DServiceTierType.ServiceTierType
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data CancelRideTagData = CancelRideTagData
  { ride :: DRide.Ride,
    booking :: DRB.Booking,
    cancellationReason :: DBCR.BookingCancellationReason,
    callAtemptByDriver :: Bool,
    currentTime :: Int,
    rideCreatedTime :: Int,
    merchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    driverArrivalTime :: Maybe Int
  }
  deriving (Generic, ToJSON)

-- ToJSON instance for Ride (orphan instance since Ride is in read-only module)
instance ToJSON DRide.Ride where
  toJSON ride =
    A.object
      [ "id" A..= ride.id.getId,
        "shortId" A..= ride.shortId.getShortId,
        "allowedEditLocationAttempts" A..= ride.allowedEditLocationAttempts,
        "allowedEditPickupLocationAttempts" A..= ride.allowedEditPickupLocationAttempts,
        "backendAppVersion" A..= ride.backendAppVersion,
        "backendConfigVersion" A..= ride.backendConfigVersion,
        "billingCategory" A..= T.pack (show ride.billingCategory),
        "bookingId" A..= ride.bookingId.getId,
        "bppRideId" A..= ride.bppRideId.getId,
        "cancellationChargesOnCancel" A..= ride.cancellationChargesOnCancel,
        "cancellationFeeIfCancelled" A..= ride.cancellationFeeIfCancelled,
        "chargeableDistance" A..= ride.chargeableDistance,
        "clientBundleVersion" A..= ride.clientBundleVersion,
        "clientConfigVersion" A..= ride.clientConfigVersion,
        "clientDevice" A..= ride.clientDevice,
        "clientId" A..= fmap (.getId) ride.clientId,
        "clientSdkVersion" A..= ride.clientSdkVersion,
        "commission" A..= ride.commission,
        "createdAt" A..= ride.createdAt,
        "destinationReachedAt" A..= ride.destinationReachedAt,
        "distanceUnit" A..= T.pack (show ride.distanceUnit),
        "driverAccountId" A..= fmap (T.pack . show) ride.driverAccountId,
        -- Skip encrypted fields: driverAlternateNumber, driverPhoneNumber (no ToJSON instance)
        "driverArrivalTime" A..= ride.driverArrivalTime,
        "driverImage" A..= ride.driverImage,
        "driverMobileCountryCode" A..= ride.driverMobileCountryCode,
        "driverMobileNumber" A..= ride.driverMobileNumber,
        "driverName" A..= ride.driverName,
        "driverRating" A..= ride.driverRating,
        "driverRegisteredAt" A..= ride.driverRegisteredAt,
        "driversPreviousRideDropLoc" A..= ride.driversPreviousRideDropLoc,
        "endOdometerReading" A..= ride.endOdometerReading,
        "endOtp" A..= ride.endOtp,
        "estimatedEndTimeRange" A..= ride.estimatedEndTimeRange,
        "fare" A..= ride.fare,
        "favCount" A..= ride.favCount,
        "feedbackSkipped" A..= ride.feedbackSkipped,
        "fromLocation" A..= ride.fromLocation,
        "hasStops" A..= ride.hasStops,
        "insuredAmount" A..= ride.insuredAmount,
        "isAlreadyFav" A..= ride.isAlreadyFav,
        "isFreeRide" A..= ride.isFreeRide,
        "isInsured" A..= ride.isInsured,
        "isPetRide" A..= ride.isPetRide,
        "isSafetyPlus" A..= ride.isSafetyPlus,
        "merchantId" A..= fmap (.getId) ride.merchantId,
        "merchantOperatingCityId" A..= fmap (.getId) ride.merchantOperatingCityId,
        "onlinePayment" A..= ride.onlinePayment,
        "otp" A..= ride.otp,
        "paymentStatus" A..= ride.paymentStatus,
        "pickupEtaLogicVersion" A..= ride.pickupEtaLogicVersion,
        "pickupRouteCallCount" A..= ride.pickupRouteCallCount,
        "pickupSpeedInMPS" A..= ride.pickupSpeedInMPS,
        "refundRequestStatus" A..= ride.refundRequestStatus,
        "rideEndTime" A..= ride.rideEndTime,
        "rideRating" A..= ride.rideRating,
        "rideStartTime" A..= ride.rideStartTime,
        "safetyCheckStatus" A..= ride.safetyCheckStatus,
        "safetyJourneyStatus" A..= ride.safetyJourneyStatus,
        "showDriversPreviousRideDropLoc" A..= ride.showDriversPreviousRideDropLoc,
        "startOdometerReading" A..= ride.startOdometerReading,
        "status" A..= T.pack (show ride.status),
        "stops" A..= ride.stops,
        "talkedWithDriver" A..= ride.talkedWithDriver,
        "tipAmount" A..= ride.tipAmount,
        "toLocation" A..= ride.toLocation,
        "tollConfidence" A..= ride.tollConfidence,
        "totalFare" A..= ride.totalFare,
        "trackingUrl" A..= ride.trackingUrl,
        "traveledDistance" A..= ride.traveledDistance,
        "updatedAt" A..= ride.updatedAt,
        "vehicleAge" A..= ride.vehicleAge,
        "vehicleColor" A..= ride.vehicleColor,
        "vehicleModel" A..= ride.vehicleModel,
        "vehicleNumber" A..= ride.vehicleNumber,
        "vehicleServiceTierType" A..= ride.vehicleServiceTierType,
        "vehicleVariant" A..= T.pack (show ride.vehicleVariant),
        "wasRideSafe" A..= ride.wasRideSafe
      ]

$(YTH.generateGenericDefault ''LoginTagData)
$(YTH.generateGenericDefault ''RideData)
$(YTH.generateGenericDefault ''SearchRequestData)
$(YTH.generateGenericDefault ''BookingData)
$(YTH.generateGenericDefault ''CustomerData)
$(YTH.generateGenericDefault ''EndRideOffersTagData)

instance YTC.LogicInputLink YA.ApplicationEvent where
  getLogicInputDef a =
    case a of
      YA.Login -> fmap A.toJSON . listToMaybe $ YTH.genDef (Proxy @LoginTagData)
      YA.RideEndOffers -> fmap A.toJSON . listToMaybe $ YTH.genDef (Proxy @EndRideOffersTagData)
      YA.RideCancel -> Nothing
      _ -> Nothing
