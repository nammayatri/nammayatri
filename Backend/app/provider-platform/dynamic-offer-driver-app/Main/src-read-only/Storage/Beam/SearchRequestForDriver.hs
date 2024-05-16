{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SearchRequestForDriver where

import qualified Data.Time
import qualified Database.Beam as B
import qualified Domain.Types.DriverInformation
import qualified Domain.Types.SearchRequestForDriver
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.Vehicle
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Version
import Tools.Beam.UtilsTH

data SearchRequestForDriverT f = SearchRequestForDriverT
  { acceptanceRatio :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    actualDistanceToPickup :: B.C f Kernel.Types.Common.Meters,
    airConditioned :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    backendAppVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    backendConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    baseFare :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money),
    baseFareAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    batchNumber :: B.C f Kernel.Prelude.Int,
    cancellationRatio :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    clientBundleVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientConfigVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientOsType :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Version.DeviceType),
    clientOsVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    clientSdkVersion :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Data.Time.LocalTime,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    customerCancellationDues :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    driverAvailableTime :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    driverDefaultStepFee :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money),
    driverDefaultStepFeeAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    driverId :: B.C f Kernel.Prelude.Text,
    driverMaxExtraFee :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money),
    driverMaxExtraFeeAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    driverMinExtraFee :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money),
    driverMinExtraFeeAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    driverSpeed :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    driverStepFee :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Money),
    driverStepFeeAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    durationToPickup :: B.C f Kernel.Types.Common.Seconds,
    estimateId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    goHomeRequestId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    isPartOfIntelligentPool :: B.C f Kernel.Prelude.Bool,
    keepHiddenForSeconds :: B.C f Kernel.Types.Common.Seconds,
    lat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    lon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    mode :: B.C f (Kernel.Prelude.Maybe Domain.Types.DriverInformation.DriverMode),
    parallelSearchRequestCount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    pickupZone :: B.C f Kernel.Prelude.Bool,
    requestId :: B.C f Kernel.Prelude.Text,
    response :: B.C f (Kernel.Prelude.Maybe Domain.Types.SearchRequestForDriver.SearchRequestForDriverResponse),
    rideFrequencyScore :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    rideRequestPopupDelayDuration :: B.C f Kernel.Types.Common.Seconds,
    searchRequestValidTill :: B.C f Data.Time.LocalTime,
    searchTryId :: B.C f Kernel.Prelude.Text,
    startTime :: B.C f Kernel.Prelude.UTCTime,
    status :: B.C f Domain.Types.SearchRequestForDriver.DriverSearchRequestStatus,
    straightLineDistanceToPickup :: B.C f Kernel.Types.Common.Meters,
    vehicleServiceTier :: B.C f (Kernel.Prelude.Maybe Domain.Types.ServiceTierType.ServiceTierType),
    vehicleServiceTierName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleVariant :: B.C f Domain.Types.Vehicle.Variant
  }
  deriving (Generic, B.Beamable)

instance B.Table SearchRequestForDriverT where
  data PrimaryKey SearchRequestForDriverT f = SearchRequestForDriverId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SearchRequestForDriverId . id

type SearchRequestForDriver = SearchRequestForDriverT Identity

$(enableKVPG ''SearchRequestForDriverT ['id] [])

$(mkTableInstancesWithTModifier ''SearchRequestForDriverT "search_request_for_driver" [("requestId", "search_request_id")])
