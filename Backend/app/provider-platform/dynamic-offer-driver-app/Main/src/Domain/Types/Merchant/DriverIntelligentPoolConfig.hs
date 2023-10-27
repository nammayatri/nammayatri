{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Merchant.DriverIntelligentPoolConfig where

import Data.Time (UTCTime)
import Domain.Types.Common
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.MerchantOperatingCity
import EulerHS.Prelude hiding (id)
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Kernel.Types.SlidingWindowCounters as SWC

data DriverIntelligentPoolConfigD u = DriverIntelligentPoolConfig
  { merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    actualPickupDistanceWeightage :: Int,
    availabilityTimeWeightage :: Int,
    availabilityTimeWindowOption :: SWC.SlidingWindowOptions,
    acceptanceRatioWeightage :: Int,
    acceptanceRatioWindowOption :: SWC.SlidingWindowOptions,
    cancellationRatioWeightage :: Int,
    cancellationRatioWindowOption :: SWC.SlidingWindowOptions,
    minQuotesToQualifyForIntelligentPool :: Int,
    minQuotesToQualifyForIntelligentPoolWindowOption :: SWC.SlidingWindowOptions,
    intelligentPoolPercentage :: Maybe Int,
    speedNormalizer :: Double, -- abnormally high speed
    driverSpeedWeightage :: Int,
    minLocationUpdates :: Int,
    locationUpdateSampleTime :: Minutes,
    defaultDriverSpeed :: Double,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)

type DriverIntelligentPoolConfig = DriverIntelligentPoolConfigD 'Safe

instance FromJSON (DriverIntelligentPoolConfigD 'Unsafe)

instance ToJSON (DriverIntelligentPoolConfigD 'Unsafe)

data IntelligentFactors = AcceptanceRatio | CancellationRatio | AvailableTime | DriverSpeed | ActualPickupDistance

data IntelligentScores = IntelligentScores
  { acceptanceRatio :: Maybe Double,
    cancellationRatio :: Maybe Double,
    availableTime :: Maybe Double,
    driverSpeed :: Maybe Double,
    actualPickupDistanceScore :: Maybe Double,
    rideRequestPopupDelayDuration :: Seconds
  }
  deriving (Generic, Show, ToJSON, FromJSON)
