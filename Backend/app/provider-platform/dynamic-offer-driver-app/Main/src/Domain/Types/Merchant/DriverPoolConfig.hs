{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Merchant.DriverPoolConfig where

-- import Data.Time (UTCTime)

import Data.Aeson
import Data.Aeson.Key as DAK
import Data.Aeson.Types
import Data.Text as Text
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Domain.Types.Common
import Domain.Types.Merchant (Merchant)
import Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Vehicle.Variant as DVeh
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude as KP
import Kernel.Types.Common
import Kernel.Types.Id
-- import qualified Data.ByteString.Lazy.Char8 as B
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool.Config

data DriverPoolConfigD u = DriverPoolConfig
  { id :: Id DriverPoolConfig,
    merchantId :: Id Merchant,
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

-- instance FromJSON DriverPoolConfig where
--   parseJSON (Object v) = do
--     -- currentTime <- DTC.getCurrentTime
-- DriverPoolConfig
-- <$> ((v.: DAK.fromText (Text.pack "id")) )
-- <*> ((v.: DAK.fromText (Text.pack "merchantId")) )
-- <*> ((v.: DAK.fromText (Text.pack "merchantOperatingCityId")) )
-- <*>  ( (v.: DAK.fromText (Text.pack "minRadiusOfSearch")))
-- <*>  ( (v.: DAK.fromText (Text.pack "maxRadiusOfSearch")))
-- <*>  ( (v.: DAK.fromText (Text.pack "radiusStepSize")))
-- <*>  (v.: DAK.fromText (Text.pack "driverPositionInfoExpiry"))
-- <*>  ( (v.: DAK.fromText (Text.pack "actualDistanceThreshold")))
-- <*>  ( (v.: DAK.fromText (Text.pack "maxDriverQuotesRequired")))
-- <*>  ( (v.: DAK.fromText (Text.pack "driverQuoteLimit")))
-- <*>  ( (v.: DAK.fromText (Text.pack "driverRequestCountLimit")))
-- <*>  ( (v.: DAK.fromText (Text.pack "driverBatchSize")))
-- <*> (v.: DAK.fromText (Text.pack "distanceBasedBatchSplit")) -- Think about something
-- <*>  ( (v.: DAK.fromText (Text.pack "maxNumberOfBatches")))
-- <*>  ( (v.: DAK.fromText (Text.pack "maxParallelSearchRequests")))
-- <*> (v.: DAK.fromText (Text.pack "poolSortingType")) -- Think about something
-- <*>  ( (v.: DAK.fromText (Text.pack "singleBatchProcessTime")))
-- <*>  ( (v.: DAK.fromText (Text.pack "tripDistance")))
-- <*>  ( (v.: DAK.fromText (Text.pack "radiusShrinkValueForDriversOnRide")))
-- <*>  ( (v.: DAK.fromText (Text.pack "driverToDestinationDistanceThreshold")))
-- <*>  ( (v.: DAK.fromText (Text.pack "driverToDestinationDuration")))
-- <*> ( (v.: DAK.fromText (Text.pack "createdAt")))
-- <*>  ( (v.: DAK.fromText (Text.pack "updatedAt")))
-- <*> (v.: DAK.fromText (Text.pack "vehicleVariant")) -- Think about something
--   parseJSON _ = mzero

jsonToDriverPoolConfig :: Object -> (Parser DriverPoolConfig)
jsonToDriverPoolConfig v =
  DriverPoolConfig
    <$> (Id <$> ((v .: DAK.fromText (Text.pack "id"))))
    <*> (Id <$> (v .: DAK.fromText (Text.pack "merchantId")))
    <*> (Id <$> (v .: DAK.fromText (Text.pack "merchantOperatingCityId")))
    <*> (Meters <$> KP.read <$> (v .: DAK.fromText (Text.pack "minRadiusOfSearch")))
    <*> (Meters <$> KP.read <$> (v .: DAK.fromText (Text.pack "maxRadiusOfSearch")))
    <*> (Meters <$> KP.read <$> (v .: DAK.fromText (Text.pack "radiusStepSize")))
    <*> ((Seconds <$>) <$> KP.readMaybe <$> (v .: DAK.fromText (Text.pack "driverPositionInfoExpiry")))
    <*> ((Meters <$>) <$> KP.readMaybe <$> (v .: DAK.fromText (Text.pack "actualDistanceThreshold")))
    <*> ((KP.read :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "maxDriverQuotesRequired")))
    <*> ((KP.read :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "driverQuoteLimit")))
    <*> ((KP.read :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "driverRequestCountLimit")))
    <*> ((KP.read :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "driverBatchSize")))
    <*> ((KP.read :: (String -> [BatchSplitByPickupDistance])) <$> (v .: DAK.fromText (Text.pack "distanceBasedBatchSplit")))
    <*> ((KP.read :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "maxNumberOfBatches")))
    <*> ((KP.read :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "maxParallelSearchRequests")))
    <*> ((KP.read :: (String -> PoolSortingType)) <$> v .: DAK.fromText (Text.pack "poolSortingType")) -- Think about something
    <*> (Seconds <$> KP.read <$> (v .: DAK.fromText (Text.pack "singleBatchProcessTime")))
    <*> (Meters <$> KP.read <$> (v .: DAK.fromText (Text.pack "tripDistance")))
    <*> (Meters <$> KP.read <$> (v .: DAK.fromText (Text.pack "radiusShrinkValueForDriversOnRide")))
    <*> (Meters <$> KP.read <$> (v .: DAK.fromText (Text.pack "driverToDestinationDistanceThreshold")))
    <*> (Seconds <$> KP.read <$> (v .: DAK.fromText (Text.pack "driverToDestinationDuration")))
    <*> (pure (posixSecondsToUTCTime 0))
    <*> (pure (posixSecondsToUTCTime 0))
    <*> (pure Nothing) -- Think about something

instance FromJSON (DriverPoolConfigD 'Unsafe)

instance ToJSON (DriverPoolConfigD 'Unsafe)
