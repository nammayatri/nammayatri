{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Yudhishthira where

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import Domain.Types.Client
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as DRide
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Types.Version
import qualified Lib.Yudhishthira.Types.Application as YA
import qualified Lib.Yudhishthira.Types.Common as YTC
import qualified Lib.Yudhishthira.TypesTH as YTH

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
$(YTH.generateGenericDefault ''CustomerData)
$(YTH.generateGenericDefault ''EndRideOffersTagData)

instance YTC.LogicInputLink YA.ApplicationEvent where
  getLogicInputDef a =
    case a of
      YA.Login -> fmap A.toJSON . listToMaybe $ YTH.genDef (Proxy @LoginTagData)
      YA.RideEndOffers -> fmap A.toJSON . listToMaybe $ YTH.genDef (Proxy @EndRideOffersTagData)
      YA.RideCancel -> Nothing
      _ -> Nothing
