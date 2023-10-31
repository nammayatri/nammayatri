{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Merchant.DriverPoolConfig where

import Data.Time (UTCTime)
import Domain.Types.Common
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Vehicle.Variant as DVeh
import EulerHS.Prelude hiding (id)
import Kernel.Types.Common
import Kernel.Types.Id
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config

data DriverPoolConfigD u = DriverPoolConfig
  { merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    minRadiusOfSearch :: Meters,
    maxRadiusOfSearch :: Meters,
    radiusStepSize :: Meters,
    driverPositionInfoExpiry :: Maybe Seconds,
    actualDistanceThreshold :: Maybe Meters,
    maxDriverQuotesRequired :: Int,
    driverQuoteLimit :: Int,
    driverRequestCountLimit :: Int,
    driverBatchSize :: Int,
    distanceBasedBatchSplit :: [BatchSplitByPickupDistance],
    maxNumberOfBatches :: Int,
    maxParallelSearchRequests :: Int,
    poolSortingType :: PoolSortingType,
    singleBatchProcessTime :: Seconds,
    singleBatchProcessTimeRental :: Seconds,
    tripDistance :: Meters,
    radiusShrinkValueForDriversOnRide :: Meters,
    driverToDestinationDistanceThreshold :: Meters,
    driverToDestinationDuration :: Seconds,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    vehicleVariant :: Maybe DVeh.Variant
  }
  deriving (Generic, Show)

type DriverPoolConfig = DriverPoolConfigD 'Safe

instance FromJSON (DriverPoolConfigD 'Unsafe)

instance ToJSON (DriverPoolConfigD 'Unsafe)
