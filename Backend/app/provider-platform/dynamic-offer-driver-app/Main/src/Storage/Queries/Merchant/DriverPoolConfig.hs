{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Merchant.DriverPoolConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant.DriverPoolConfig
import Domain.Types.Merchant.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.DriverPoolConfig as BeamDPC

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DriverPoolConfig -> m ()
create = createWithKV

findAllByMerchantOpCityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id MerchantOperatingCity -> m [DriverPoolConfig]
findAllByMerchantOpCityId (Id merchantOperatingCityId) = findAllWithOptionsKV [Se.Is BeamDPC.merchantOperatingCityId $ Se.Eq merchantOperatingCityId] (Se.Desc BeamDPC.tripDistance) Nothing Nothing

findByMerchantIdAndTripDistance :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id MerchantOperatingCity -> Meters -> m (Maybe DriverPoolConfig)
findByMerchantIdAndTripDistance (Id merchantOperatingCityId) tripDistance = findOneWithKV [Se.And [Se.Is BeamDPC.merchantOperatingCityId $ Se.Eq merchantOperatingCityId, Se.Is BeamDPC.tripDistance $ Se.Eq tripDistance]]

update :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => DriverPoolConfig -> m ()
update config = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamDPC.minRadiusOfSearch config.minRadiusOfSearch,
      Se.Set BeamDPC.maxRadiusOfSearch config.maxRadiusOfSearch,
      Se.Set BeamDPC.radiusStepSize config.radiusStepSize,
      Se.Set BeamDPC.driverPositionInfoExpiry config.driverPositionInfoExpiry,
      Se.Set BeamDPC.actualDistanceThreshold config.actualDistanceThreshold,
      Se.Set BeamDPC.maxDriverQuotesRequired config.maxDriverQuotesRequired,
      Se.Set BeamDPC.driverQuoteLimit config.driverQuoteLimit,
      Se.Set BeamDPC.driverRequestCountLimit config.driverRequestCountLimit,
      Se.Set BeamDPC.driverBatchSize config.driverBatchSize,
      Se.Set BeamDPC.maxNumberOfBatches config.maxNumberOfBatches,
      Se.Set BeamDPC.maxParallelSearchRequests config.maxParallelSearchRequests,
      Se.Set BeamDPC.poolSortingType config.poolSortingType,
      Se.Set BeamDPC.singleBatchProcessTime config.singleBatchProcessTime,
      Se.Set BeamDPC.updatedAt now
    ]
    [Se.And [Se.Is BeamDPC.merchantOperatingCityId (Se.Eq $ getId config.merchantOperatingCityId), Se.Is BeamDPC.tripDistance (Se.Eq config.tripDistance)]]

instance FromTType' BeamDPC.DriverPoolConfig DriverPoolConfig where
  fromTType' BeamDPC.DriverPoolConfigT {..} = do
    pure $
      Just
        DriverPoolConfig
          { id = Id id,
            merchantId = Id merchantId,
            merchantOperatingCityId = Id merchantOperatingCityId,
            distanceBasedBatchSplit = distanceBasedBatchSplit,
            minRadiusOfSearch = minRadiusOfSearch,
            maxRadiusOfSearch = maxRadiusOfSearch,
            radiusStepSize = radiusStepSize,
            driverPositionInfoExpiry = driverPositionInfoExpiry,
            actualDistanceThreshold = actualDistanceThreshold,
            maxDriverQuotesRequired = maxDriverQuotesRequired,
            driverQuoteLimit = driverQuoteLimit,
            driverRequestCountLimit = driverRequestCountLimit,
            driverBatchSize = driverBatchSize,
            maxNumberOfBatches = maxNumberOfBatches,
            maxParallelSearchRequests = maxParallelSearchRequests,
            poolSortingType = poolSortingType,
            singleBatchProcessTime = singleBatchProcessTime,
            tripDistance = tripDistance,
            radiusShrinkValueForDriversOnRide = radiusShrinkValueForDriversOnRide,
            driverToDestinationDistanceThreshold = driverToDestinationDistanceThreshold,
            driverToDestinationDuration = driverToDestinationDuration,
            createdAt = createdAt,
            updatedAt = updatedAt,
            vehicleVariant = vehicleVariant
          }

instance ToTType' BeamDPC.DriverPoolConfig DriverPoolConfig where
  toTType' DriverPoolConfig {..} = do
    BeamDPC.DriverPoolConfigT
      { BeamDPC.id = getId id,
        BeamDPC.merchantId = getId merchantId,
        BeamDPC.merchantOperatingCityId = getId merchantOperatingCityId,
        BeamDPC.distanceBasedBatchSplit = distanceBasedBatchSplit,
        BeamDPC.minRadiusOfSearch = minRadiusOfSearch,
        BeamDPC.maxRadiusOfSearch = maxRadiusOfSearch,
        BeamDPC.radiusStepSize = radiusStepSize,
        BeamDPC.driverPositionInfoExpiry = driverPositionInfoExpiry,
        BeamDPC.actualDistanceThreshold = actualDistanceThreshold,
        BeamDPC.maxDriverQuotesRequired = maxDriverQuotesRequired,
        BeamDPC.driverQuoteLimit = driverQuoteLimit,
        BeamDPC.driverRequestCountLimit = driverRequestCountLimit,
        BeamDPC.driverBatchSize = driverBatchSize,
        BeamDPC.maxNumberOfBatches = maxNumberOfBatches,
        BeamDPC.maxParallelSearchRequests = maxParallelSearchRequests,
        BeamDPC.poolSortingType = poolSortingType,
        BeamDPC.singleBatchProcessTime = singleBatchProcessTime,
        BeamDPC.tripDistance = tripDistance,
        BeamDPC.radiusShrinkValueForDriversOnRide = radiusShrinkValueForDriversOnRide,
        BeamDPC.driverToDestinationDistanceThreshold = driverToDestinationDistanceThreshold,
        BeamDPC.driverToDestinationDuration = driverToDestinationDuration,
        BeamDPC.vehicleVariant = vehicleVariant,
        BeamDPC.createdAt = createdAt,
        BeamDPC.updatedAt = updatedAt
      }
