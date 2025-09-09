{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SearchRequestForDriver where

import qualified Data.Aeson
import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Common
import qualified Domain.Types.ParcelType
import qualified Domain.Types.SearchRequestForDriver
import qualified Domain.Types.VehicleCategory
import qualified Domain.Types.VehicleVariant
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Time
import qualified Kernel.Types.Version
import Tools.Beam.UtilsTH

data SearchRequestForDriverT f = SearchRequestForDriverT
  { acceptanceRatio :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    actualDistanceToPickup :: (B.C f Kernel.Types.Common.Meters),
    airConditioned :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    backendAppVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    backendConfigVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    baseFare :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money)),
    baseFareAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    batchNumber :: (B.C f Kernel.Prelude.Int),
    cancellationRatio :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    clientBundleVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    clientConfigVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    clientManufacturer :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    clientModelName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    clientOsType :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Version.DeviceType)),
    clientOsVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    clientSdkVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    coinsRewardedOnGoldTierRide :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    conditionalCharges :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Data.Time.LocalTime),
    currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
    customerCancellationDues :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    customerTags :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    distanceUnit :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit)),
    driverAvailableTime :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    driverDefaultStepFee :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money)),
    driverDefaultStepFeeAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    driverId :: (B.C f Kernel.Prelude.Text),
    driverMaxExtraFee :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money)),
    driverMaxExtraFeeAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    driverMinExtraFee :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money)),
    driverMinExtraFeeAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    driverSpeed :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    driverStepFee :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money)),
    driverStepFeeAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    driverTagScore :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    driverTags :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    durationToPickup :: (B.C f Kernel.Types.Common.Seconds),
    estimateId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    fromLocGeohash :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    goHomeRequestId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    id :: (B.C f Kernel.Prelude.Text),
    isFavourite :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    isForwardRequest :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    isPartOfIntelligentPool :: (B.C f Kernel.Prelude.Bool),
    isSafetyPlus :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    keepHiddenForSeconds :: (B.C f Kernel.Types.Common.Seconds),
    lat :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    lon :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    middleStopCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    mode :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Common.DriverMode)),
    notificationSource :: (B.C f (Kernel.Prelude.Maybe Domain.Types.SearchRequestForDriver.NotificationSource)),
    parallelSearchRequestCount :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    parcelQuantity :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    parcelType :: (B.C f (Kernel.Prelude.Maybe Domain.Types.ParcelType.ParcelType)),
    pickupZone :: (B.C f Kernel.Prelude.Bool),
    poolingConfigVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    poolingLogicVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    previousDropGeoHash :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    reactBundleVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    renderedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    requestId :: (B.C f Kernel.Prelude.Text),
    respondedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    response :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Common.SearchRequestForDriverResponse)),
    rideFrequencyScore :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double)),
    rideRequestPopupDelayDuration :: (B.C f Kernel.Types.Common.Seconds),
    searchRequestValidTill :: (B.C f Data.Time.LocalTime),
    searchTryId :: (B.C f Kernel.Prelude.Text),
    startTime :: (B.C f Kernel.Prelude.UTCTime),
    status :: (B.C f Domain.Types.SearchRequestForDriver.DriverSearchRequestStatus),
    straightLineDistanceToPickup :: (B.C f Kernel.Types.Common.Meters),
    totalRides :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    tripEstimatedDistance :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Meters)),
    tripEstimatedDuration :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Seconds)),
    updatedAt :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    upgradeCabRequest :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    vehicleAge :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Time.Months)),
    vehicleCategory :: (B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory)),
    vehicleServiceTier :: (B.C f (Kernel.Prelude.Maybe Domain.Types.Common.ServiceTierType)),
    vehicleServiceTierName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    vehicleVariant :: (B.C f Domain.Types.VehicleVariant.VehicleVariant)
  }
  deriving (Generic, B.Beamable)

instance B.Table SearchRequestForDriverT where
  data PrimaryKey SearchRequestForDriverT f = SearchRequestForDriverId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SearchRequestForDriverId . id

type SearchRequestForDriver = SearchRequestForDriverT Identity

$(enableKVPG (''SearchRequestForDriverT) [('id)] [[('requestId)], [('searchTryId)]])

$(mkTableInstancesWithTModifier (''SearchRequestForDriverT) "search_request_for_driver" [("requestId", "search_request_id")])
