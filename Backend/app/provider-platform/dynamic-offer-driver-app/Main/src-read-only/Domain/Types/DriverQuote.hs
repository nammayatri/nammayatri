{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverQuote where

import Data.Aeson
import qualified Domain.Types.Common
import qualified Domain.Types.DriverGoHomeRequest
import qualified Domain.Types.Estimate
import qualified Domain.Types.FareParameters
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.SearchRequest
import qualified Domain.Types.SearchRequestForDriver
import qualified Domain.Types.SearchTry
import qualified Domain.Types.VehicleVariant
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.Version
import qualified Tools.Beam.UtilsTH

data DriverQuote = DriverQuote
  { backendAppVersion :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    backendConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientBundleVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientConfigVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    clientDevice :: Kernel.Prelude.Maybe Kernel.Types.Version.Device,
    clientId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    clientSdkVersion :: Kernel.Prelude.Maybe Kernel.Types.Version.Version,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    distance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    distanceToPickup :: Kernel.Types.Common.Meters,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    driverName :: Kernel.Prelude.Text,
    driverRating :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    durationToPickup :: Kernel.Types.Common.Seconds,
    estimateId :: Kernel.Types.Id.Id Domain.Types.Estimate.Estimate,
    estimatedFare :: Kernel.Types.Common.HighPrecMoney,
    fareParams :: Domain.Types.FareParameters.FareParameters,
    goHomeRequestId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.DriverGoHomeRequest.DriverGoHomeRequest),
    id :: Kernel.Types.Id.Id Domain.Types.DriverQuote.DriverQuote,
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    providerId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    requestId :: Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest,
    searchRequestForDriverId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.SearchRequestForDriver.SearchRequestForDriver),
    searchTryId :: Kernel.Types.Id.Id Domain.Types.SearchTry.SearchTry,
    specialLocationTag :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Domain.Types.DriverQuote.DriverQuoteStatus,
    tripCategory :: Domain.Types.Common.TripCategory,
    updatedAt :: Kernel.Prelude.UTCTime,
    validTill :: Kernel.Prelude.UTCTime,
    vehicleServiceTier :: Domain.Types.Common.ServiceTierType,
    vehicleServiceTierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleVariant :: Domain.Types.VehicleVariant.VehicleVariant
  }
  deriving (Generic, Show)

data DriverQuoteStatus = Active | Inactive deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''DriverQuoteStatus)
