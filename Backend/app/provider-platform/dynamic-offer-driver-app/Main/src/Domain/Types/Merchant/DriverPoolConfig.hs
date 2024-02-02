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
-- import Data.Time.Clock.POSIX
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
readWithInfo :: (Read a, Show a) => String -> a
readWithInfo s = case KP.readMaybe s of
  Just val -> val
  Nothing -> error . Text.pack $ "Failed to parse: " ++ s

jsonToDriverPoolConfig :: Object -> (Parser DriverPoolConfig)
jsonToDriverPoolConfig v =
  DriverPoolConfig
    <$> (Id <$> ((v .: DAK.fromText (Text.pack "driverPoolConfig:id"))))
    <*> (Id <$> (v .: DAK.fromText (Text.pack "driverPoolConfig:merchantId")))
    <*> (Id <$> (v .: DAK.fromText (Text.pack "driverPoolConfig:merchantOperatingCityId")))
    <*> (Meters <$> readWithInfo <$> (v .: DAK.fromText (Text.pack "driverPoolConfig:minRadiusOfSearch")))
    <*> (Meters <$> readWithInfo <$> (v .: DAK.fromText (Text.pack "driverPoolConfig:maxRadiusOfSearch")))
    <*> (Meters <$> readWithInfo <$> (v .: DAK.fromText (Text.pack "driverPoolConfig:radiusStepSize")))
    <*> ((Seconds <$>) <$> KP.readMaybe <$> (v .: DAK.fromText (Text.pack "driverPoolConfig:driverPositionInfoExpiry")))
    <*> ((Meters <$>) <$> KP.readMaybe <$> (v .: DAK.fromText (Text.pack "driverPoolConfig:actualDistanceThreshold")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "driverPoolConfig:maxDriverQuotesRequired")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "driverPoolConfig:driverQuoteLimit")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "driverPoolConfig:driverRequestCountLimit")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "driverPoolConfig:driverBatchSize")))
    <*> (((readWithInfo :: (String -> BatchSplitByPickupDistance)) <$>) <$> (v .: DAK.fromText (Text.pack "driverPoolConfig:distanceBasedBatchSplit")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "driverPoolConfig:maxNumberOfBatches")))
    <*> ((readWithInfo :: (String -> Int)) <$> (v .: DAK.fromText (Text.pack "driverPoolConfig:maxParallelSearchRequests")))
    <*> ((readWithInfo :: (String -> PoolSortingType)) <$> v .: DAK.fromText (Text.pack "driverPoolConfig:poolSortingType"))
    <*> (Seconds <$> readWithInfo <$> (v .: DAK.fromText (Text.pack "driverPoolConfig:singleBatchProcessTime")))
    <*> (Meters <$> readWithInfo <$> (v .: DAK.fromText (Text.pack "driverPoolConfig:tripDistance")))
    <*> (Meters <$> readWithInfo <$> (v .: DAK.fromText (Text.pack "driverPoolConfig:radiusShrinkValueForDriversOnRide")))
    <*> (Meters <$> readWithInfo <$> (v .: DAK.fromText (Text.pack "driverPoolConfig:driverToDestinationDistanceThreshold")))
    <*> (Seconds <$> readWithInfo <$> (v .: DAK.fromText (Text.pack "driverPoolConfig:driverToDestinationDuration")))
    <*> ((readWithInfo :: (String -> UTCTime)) <$> (v .: DAK.fromText (Text.pack "driverPoolConfig:createdAt")))
    <*> ((readWithInfo :: (String -> UTCTime)) <$> (v .: DAK.fromText (Text.pack "driverPoolConfig:updatedAt")))
    <*> ((KP.readMaybe :: (String -> Maybe DVeh.Variant)) <$> (v .: DAK.fromText (Text.pack "driverPoolConfig:vehicleVariant")))

instance FromJSON (DriverPoolConfigD 'Unsafe)

instance ToJSON (DriverPoolConfigD 'Unsafe)
