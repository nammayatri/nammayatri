{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Yudhishthira where

import qualified Data.Aeson as A
import Domain.Types.Client
import Domain.Types.Person
import qualified Domain.Types.Person as SP
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

$(YTH.generateGenericDefault ''LoginTagData)
$(YTH.generateGenericDefault ''RideData)
$(YTH.generateGenericDefault ''CustomerData)
$(YTH.generateGenericDefault ''EndRideOffersTagData)

instance YTC.LogicInputLink YA.ApplicationEvent where
  getLogicInputDef a =
    case a of
      YA.Login -> fmap A.toJSON . listToMaybe $ YTH.genDef (Proxy @LoginTagData)
      YA.RideEndOffers -> fmap A.toJSON . listToMaybe $ YTH.genDef (Proxy @EndRideOffersTagData)
      _ -> Nothing
