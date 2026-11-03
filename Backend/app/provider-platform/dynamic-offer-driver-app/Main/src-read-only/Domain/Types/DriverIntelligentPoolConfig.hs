{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverIntelligentPoolConfig where

import Data.Aeson
import Domain.Types.Common (UsageSafety (..))
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters
import qualified Tools.Beam.UtilsTH

data DriverIntelligentPoolConfigD (s :: UsageSafety) = DriverIntelligentPoolConfig
  { acceptanceRatioWeightage :: Kernel.Prelude.Int,
    acceptanceRatioWindowOption :: Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    actualPickupDistanceWeightage :: Kernel.Prelude.Int,
    availabilityTimeWeightage :: Kernel.Prelude.Int,
    availabilityTimeWindowOption :: Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    cancellationAndRideFrequencyRatioWindowOption :: Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    cancellationRatioWeightage :: Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    defaultDriverSpeed :: Kernel.Prelude.Double,
    driverSpeedWeightage :: Kernel.Prelude.Int,
    intelligentPoolPercentage :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    locationUpdateSampleTime :: Kernel.Types.Common.Minutes,
    maxNumRides :: Kernel.Prelude.Int,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    minLocationUpdates :: Kernel.Prelude.Int,
    minQuotesToQualifyForIntelligentPool :: Kernel.Prelude.Int,
    minQuotesToQualifyForIntelligentPoolWindowOption :: Kernel.Types.SlidingWindowCounters.SlidingWindowOptions,
    numRidesWeightage :: Kernel.Prelude.Int,
    speedNormalizer :: Kernel.Prelude.Double,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)

data IntelligentFactors = AcceptanceRatio | CancellationRatio | AvailableTime | DriverSpeed | ActualPickupDistance | RideFrequency deriving (Generic, Show, ToJSON, FromJSON, Read, Eq)

data IntelligentScores = IntelligentScores
  { acceptanceRatio :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    actualPickupDistanceScore :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    availableTime :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    cancellationRatio :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    driverSpeed :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    rideFrequency :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    rideRequestPopupDelayDuration :: Kernel.Types.Common.Seconds
  }
  deriving (Generic, Show, ToJSON, FromJSON, Read)

type DriverIntelligentPoolConfig = DriverIntelligentPoolConfigD 'Safe

instance FromJSON (DriverIntelligentPoolConfigD 'Unsafe)

instance ToJSON (DriverIntelligentPoolConfigD 'Unsafe)

instance FromJSON (DriverIntelligentPoolConfigD 'Safe)

instance ToJSON (DriverIntelligentPoolConfigD 'Safe)
