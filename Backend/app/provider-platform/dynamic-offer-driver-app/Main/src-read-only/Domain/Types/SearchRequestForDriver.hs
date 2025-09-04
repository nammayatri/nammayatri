{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SearchRequestForDriver where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.ConditionalCharges
import qualified Domain.Types.DriverGoHomeRequest
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ParcelType
import qualified Domain.Types.Person
import qualified Domain.Types.SearchRequest
import qualified Domain.Types.SearchTry
import qualified Domain.Types.VehicleCategory
import qualified Domain.Types.VehicleVariant
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Time
import qualified Kernel.Types.Version
import qualified Tools.Beam.UtilsTH

data SearchRequestForDriver = SearchRequestForDriver
  { acceptanceRatio :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    actualDistanceToPickup :: Kernel.Types.Common.Meters,
    airConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    backendAppVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    backendConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    baseFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    batchNumber :: Kernel.Prelude.Int,
    cancellationRatio :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    clientBundleVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientDevice :: Kernel.Prelude.Maybe Kernel.Types.Version.Device,
    clientSdkVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    coinsRewardedOnGoldTierRide :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    conditionalCharges :: [Domain.Types.ConditionalCharges.ConditionalChargesCategories],
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    customerCancellationDues :: Kernel.Types.Common.HighPrecMoney,
    customerTags :: Kernel.Prelude.Maybe Data.Aeson.Value,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    driverAvailableTime :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    driverDefaultStepFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    driverMaxExtraFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    driverMinExtraFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    driverSpeed :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    driverStepFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    driverTagScore :: Kernel.Prelude.Maybe Data.Aeson.Value,
    driverTags :: Kernel.Prelude.Maybe Data.Aeson.Value,
    durationToPickup :: Kernel.Types.Common.Seconds,
    estimateId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fromLocGeohash :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    goHomeRequestId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.DriverGoHomeRequest.DriverGoHomeRequest),
    id :: Kernel.Types.Id.Id Domain.Types.SearchRequestForDriver.SearchRequestForDriver,
    isFavourite :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    isForwardRequest :: Kernel.Prelude.Bool,
    isPartOfIntelligentPool :: Kernel.Prelude.Bool,
    isSafetyPlus :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    keepHiddenForSeconds :: Kernel.Types.Common.Seconds,
    lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    middleStopCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    mode :: Kernel.Prelude.Maybe Domain.Types.Common.DriverMode,
    notificationSource :: Kernel.Prelude.Maybe Domain.Types.SearchRequestForDriver.NotificationSource,
    parallelSearchRequestCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    parcelQuantity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    parcelType :: Kernel.Prelude.Maybe Domain.Types.ParcelType.ParcelType,
    pickupZone :: Kernel.Prelude.Bool,
    poolingConfigVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    poolingLogicVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    previousDropGeoHash :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reactBundleVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    renderedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    requestId :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    respondedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    response :: Kernel.Prelude.Maybe Domain.Types.Common.SearchRequestForDriverResponse,
    rideFrequencyScore :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    rideRequestPopupDelayDuration :: Kernel.Types.Common.Seconds,
    searchRequestValidTill :: Kernel.Prelude.UTCTime,
    searchTryId :: Kernel.Types.Id.Id Domain.Types.SearchTry.SearchTry,
    startTime :: Kernel.Prelude.UTCTime,
    status :: Domain.Types.SearchRequestForDriver.DriverSearchRequestStatus,
    straightLineDistanceToPickup :: Kernel.Types.Common.Meters,
    totalRides :: Kernel.Prelude.Int,
    tripEstimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    tripEstimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    updatedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    upgradeCabRequest :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    vehicleAge :: Kernel.Prelude.Maybe Kernel.Types.Time.Months,
    vehicleCategory :: Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory,
    vehicleServiceTier :: Domain.Types.Common.ServiceTierType,
    vehicleServiceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleVariant :: Domain.Types.VehicleVariant.VehicleVariant
  }
  deriving (Generic, (Show))

data DriverSearchRequestStatus = Active | Inactive deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data NotificationSource = FCM | GRPC deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''DriverSearchRequestStatus))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''NotificationSource))
