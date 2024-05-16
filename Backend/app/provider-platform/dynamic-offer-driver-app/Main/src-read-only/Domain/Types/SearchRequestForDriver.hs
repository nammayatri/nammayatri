{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SearchRequestForDriver where

import Data.Aeson
import qualified Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest
import qualified Domain.Types.DriverInformation
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.SearchRequest
import qualified Domain.Types.SearchTry
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.Vehicle
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
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
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    customerCancellationDues :: Kernel.Types.Common.HighPrecMoney,
    driverAvailableTime :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    driverDefaultStepFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    driverMaxExtraFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    driverMinExtraFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    driverSpeed :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    driverStepFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    durationToPickup :: Kernel.Types.Common.Seconds,
    estimateId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    goHomeRequestId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest.DriverGoHomeRequest),
    id :: Kernel.Types.Id.Id Domain.Types.SearchRequestForDriver.SearchRequestForDriver,
    isPartOfIntelligentPool :: Kernel.Prelude.Bool,
    keepHiddenForSeconds :: Kernel.Types.Common.Seconds,
    lat :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity,
    mode :: Kernel.Prelude.Maybe Domain.Types.DriverInformation.DriverMode,
    parallelSearchRequestCount :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    pickupZone :: Kernel.Prelude.Bool,
    requestId :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    response :: Kernel.Prelude.Maybe Domain.Types.SearchRequestForDriver.SearchRequestForDriverResponse,
    rideFrequencyScore :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    rideRequestPopupDelayDuration :: Kernel.Types.Common.Seconds,
    searchRequestValidTill :: Kernel.Prelude.UTCTime,
    searchTryId :: Kernel.Types.Id.Id Domain.Types.SearchTry.SearchTry,
    startTime :: Kernel.Prelude.UTCTime,
    status :: Domain.Types.SearchRequestForDriver.DriverSearchRequestStatus,
    straightLineDistanceToPickup :: Kernel.Types.Common.Meters,
    vehicleServiceTier :: Domain.Types.ServiceTierType.ServiceTierType,
    vehicleServiceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleVariant :: Domain.Types.Vehicle.Variant
  }
  deriving (Generic, Show)

data DriverSearchRequestStatus = Active | Inactive deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data SearchRequestForDriverResponse = Accept | Reject | Pulled deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''DriverSearchRequestStatus)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''SearchRequestForDriverResponse)
