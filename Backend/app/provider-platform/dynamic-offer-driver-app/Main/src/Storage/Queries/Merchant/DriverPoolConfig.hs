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
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Common (Meters, MonadTime (getCurrentTime))
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.DriverPoolConfig as BeamDPC

-- create :: DriverPoolConfig -> SqlDB ()
-- create = Esq.create

create :: L.MonadFlow m => DriverPoolConfig -> m (MeshResult ())
create driverPoolConfig = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' Mesh.meshConfig (transformDomainDriverPoolConfigToBeam driverPoolConfig)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- findAllByMerchantId :: Transactionable m => Id Merchant -> m [DriverPoolConfig]
-- findAllByMerchantId merchantId =
--   Esq.findAll $ do
--     driverPoolConfig <- from $ table @DriverPoolConfigT
--     where_ $
--       driverPoolConfig ^. DriverPoolConfigMerchantId ==. val (toKey merchantId)
--     orderBy [desc $ driverPoolConfig ^. DriverPoolConfigTripDistance]
--     return driverPoolConfig

findAllByMerchantId :: L.MonadFlow m => Id Merchant -> m [DriverPoolConfig]
findAllByMerchantId (Id merchantId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamDriverPoolConfigToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamDPC.merchantId $ Se.Eq merchantId]
    Nothing -> pure []

-- findByMerchantIdAndTripDistance :: Transactionable m => Id Merchant -> Meters -> m (Maybe DriverPoolConfig)
-- findByMerchantIdAndTripDistance merchantId tripDistance =
--   Esq.findOne $ do
--     driverPoolConfig <- from $ table @DriverPoolConfigT
--     where_ $
--       driverPoolConfig ^. DriverPoolConfigTId ==. val (toKey (merchantId, tripDistance))
--     return driverPoolConfig

findByMerchantIdAndTripDistance :: L.MonadFlow m => Id Merchant -> Meters -> m (Maybe DriverPoolConfig)
findByMerchantIdAndTripDistance (Id merchantId) tripDistance = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamDriverPoolConfigToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.And [Se.Is BeamDPC.merchantId $ Se.Eq merchantId, Se.Is BeamDPC.tripDistance $ Se.Eq tripDistance]]
    Nothing -> pure Nothing

-- update :: DriverPoolConfig -> SqlDB ()
-- update config = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverPoolConfigMinRadiusOfSearch =. val config.minRadiusOfSearch,
--         DriverPoolConfigMaxRadiusOfSearch =. val config.maxRadiusOfSearch,
--         DriverPoolConfigRadiusStepSize =. val config.radiusStepSize,
--         DriverPoolConfigDriverPositionInfoExpiry =. val config.driverPositionInfoExpiry,
--         DriverPoolConfigActualDistanceThreshold =. val config.actualDistanceThreshold,
--         DriverPoolConfigMaxDriverQuotesRequired =. val config.maxDriverQuotesRequired,
--         DriverPoolConfigDriverQuoteLimit =. val config.driverQuoteLimit,
--         DriverPoolConfigDriverRequestCountLimit =. val config.driverRequestCountLimit,
--         DriverPoolConfigDriverBatchSize =. val config.driverBatchSize,
--         DriverPoolConfigMaxNumberOfBatches =. val config.maxNumberOfBatches,
--         DriverPoolConfigMaxParallelSearchRequests =. val config.maxParallelSearchRequests,
--         DriverPoolConfigPoolSortingType =. val config.poolSortingType,
--         DriverPoolConfigSingleBatchProcessTime =. val config.singleBatchProcessTime,
--         DriverPoolConfigUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. DriverPoolConfigTId ==. val (toKey (config.merchantId, config.tripDistance))

update :: (L.MonadFlow m, MonadTime m) => DriverPoolConfig -> m (MeshResult ())
update config = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
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
