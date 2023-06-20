{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Merchant.DriverPoolConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant
import Domain.Types.Merchant.DriverPoolConfig
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Types.Common (Meters, MonadTime (getCurrentTime))
import Kernel.Types.Id
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.DriverPoolConfig as BeamDPC

create :: L.MonadFlow m => DriverPoolConfig -> m (MeshResult ())
create driverPoolConfig = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDPC.DriverPoolConfigT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainDriverPoolConfigToBeam driverPoolConfig)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

findAllByMerchantId :: L.MonadFlow m => Id Merchant -> m [DriverPoolConfig]
findAllByMerchantId (Id merchantId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDPC.DriverPoolConfigT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamDriverPoolConfigToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' updatedMeshConfig [Se.Is BeamDPC.merchantId $ Se.Eq merchantId]
    Nothing -> pure []

findByMerchantIdAndTripDistance :: L.MonadFlow m => Id Merchant -> Meters -> m (Maybe DriverPoolConfig)
findByMerchantIdAndTripDistance (Id merchantId) tripDistance = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDPC.DriverPoolConfigT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamDriverPoolConfigToDomain <$>) <$> KV.findWithKVConnector dbCOnf' updatedMeshConfig [Se.And [Se.Is BeamDPC.merchantId $ Se.Eq merchantId, Se.Is BeamDPC.tripDistance $ Se.Eq tripDistance]]
    Nothing -> pure Nothing

update :: (L.MonadFlow m, MonadTime m) => DriverPoolConfig -> m (MeshResult ())
update config = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamDPC.DriverPoolConfigT
  let updatedMeshConfig = setMeshConfig modelName
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        updatedMeshConfig
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
        [Se.Is BeamDPC.merchantId (Se.Eq $ getId config.merchantId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

transformBeamDriverPoolConfigToDomain :: BeamDPC.DriverPoolConfig -> DriverPoolConfig
transformBeamDriverPoolConfigToDomain BeamDPC.DriverPoolConfigT {..} = do
  DriverPoolConfig
    { merchantId = Id merchantId,
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
      updatedAt = updatedAt
    }

transformDomainDriverPoolConfigToBeam :: DriverPoolConfig -> BeamDPC.DriverPoolConfig
transformDomainDriverPoolConfigToBeam DriverPoolConfig {..} =
  BeamDPC.DriverPoolConfigT
    { BeamDPC.merchantId = getId merchantId,
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
      BeamDPC.createdAt = createdAt,
      BeamDPC.updatedAt = updatedAt
    }
