{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Merchant.DriverIntelligentPoolConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant
import Domain.Types.Merchant.DriverIntelligentPoolConfig
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common (MonadTime (getCurrentTime))
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.DriverIntelligentPoolConfig as BeamDIPC
import Storage.Tabular.Merchant.DriverIntelligentPoolConfig

-- findByMerchantId :: Transactionable m => Id Merchant -> m (Maybe DriverIntelligentPoolConfig)
-- findByMerchantId merchantId =
--   Esq.findOne $ do
--     config <- from $ table @DriverIntelligentPoolConfigT
--     where_ $
--       config ^. DriverIntelligentPoolConfigMerchantId ==. val (toKey merchantId)
--     return config

findByMerchantId :: L.MonadFlow m => Id Merchant -> m (Maybe DriverIntelligentPoolConfig)
findByMerchantId (Id merchantId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamDriverIntelligentPoolConfigToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamDIPC.merchantId $ Se.Eq merchantId]
    Nothing -> pure Nothing

-- update :: DriverIntelligentPoolConfig -> SqlDB ()
-- update config = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverIntelligentPoolConfigAvailabilityTimeWeightage =. val config.availabilityTimeWeightage,
--         DriverIntelligentPoolConfigAvailabilityTimeWindowOption =. val config.availabilityTimeWindowOption,
--         DriverIntelligentPoolConfigAcceptanceRatioWeightage =. val config.acceptanceRatioWeightage,
--         DriverIntelligentPoolConfigAcceptanceRatioWindowOption =. val config.acceptanceRatioWindowOption,
--         DriverIntelligentPoolConfigCancellationRatioWeightage =. val config.cancellationRatioWeightage,
--         DriverIntelligentPoolConfigCancellationRatioWindowOption =. val config.cancellationRatioWindowOption,
--         DriverIntelligentPoolConfigMinQuotesToQualifyForIntelligentPool =. val config.minQuotesToQualifyForIntelligentPool,
--         DriverIntelligentPoolConfigMinQuotesToQualifyForIntelligentPoolWindowOption =. val config.minQuotesToQualifyForIntelligentPoolWindowOption,
--         DriverIntelligentPoolConfigIntelligentPoolPercentage =. val config.intelligentPoolPercentage,
--         DriverIntelligentPoolConfigSpeedNormalizer =. val config.speedNormalizer,
--         DriverIntelligentPoolConfigDriverSpeedWeightage =. val config.driverSpeedWeightage,
--         DriverIntelligentPoolConfigMinLocationUpdates =. val config.minLocationUpdates,
--         DriverIntelligentPoolConfigLocationUpdateSampleTime =. val config.locationUpdateSampleTime,
--         DriverIntelligentPoolConfigDefaultDriverSpeed =. val config.defaultDriverSpeed,
--         DriverIntelligentPoolConfigUpdatedAt =. val now
--       ]
--     where_ $ tbl ^. DriverIntelligentPoolConfigMerchantId ==. val (toKey config.merchantId)

update :: (L.MonadFlow m, MonadTime m) => DriverIntelligentPoolConfig -> m (MeshResult ())
update config = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        Mesh.meshConfig
        [ Se.Set BeamDIPC.availabilityTimeWeightage config.availabilityTimeWeightage,
          Se.Set BeamDIPC.availabilityTimeWindowOption config.availabilityTimeWindowOption,
          Se.Set BeamDIPC.acceptanceRatioWeightage config.acceptanceRatioWeightage,
          Se.Set BeamDIPC.acceptanceRatioWindowOption config.acceptanceRatioWindowOption,
          Se.Set BeamDIPC.cancellationRatioWeightage config.cancellationRatioWeightage,
          Se.Set BeamDIPC.cancellationRatioWindowOption config.cancellationRatioWindowOption,
          Se.Set BeamDIPC.minQuotesToQualifyForIntelligentPool config.minQuotesToQualifyForIntelligentPool,
          Se.Set BeamDIPC.minQuotesToQualifyForIntelligentPoolWindowOption config.minQuotesToQualifyForIntelligentPoolWindowOption,
          Se.Set BeamDIPC.intelligentPoolPercentage config.intelligentPoolPercentage,
          Se.Set BeamDIPC.speedNormalizer config.speedNormalizer,
          Se.Set BeamDIPC.driverSpeedWeightage config.driverSpeedWeightage,
          Se.Set BeamDIPC.minLocationUpdates config.minLocationUpdates,
          Se.Set BeamDIPC.locationUpdateSampleTime config.locationUpdateSampleTime,
          Se.Set BeamDIPC.defaultDriverSpeed config.defaultDriverSpeed,
          Se.Set BeamDIPC.updatedAt now
        ]
        [Se.Is BeamDIPC.merchantId (Se.Eq $ getId config.merchantId)]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

transformBeamDriverIntelligentPoolConfigToDomain :: BeamDIPC.DriverIntelligentPoolConfig -> DriverIntelligentPoolConfig
transformBeamDriverIntelligentPoolConfigToDomain BeamDIPC.DriverIntelligentPoolConfigT {..} = do
  DriverIntelligentPoolConfig
    { merchantId = Id merchantId,
      availabilityTimeWeightage = availabilityTimeWeightage,
      availabilityTimeWindowOption = availabilityTimeWindowOption,
      acceptanceRatioWeightage = acceptanceRatioWeightage,
      acceptanceRatioWindowOption = acceptanceRatioWindowOption,
      cancellationRatioWeightage = cancellationRatioWeightage,
      cancellationRatioWindowOption = cancellationRatioWindowOption,
      minQuotesToQualifyForIntelligentPool = minQuotesToQualifyForIntelligentPool,
      minQuotesToQualifyForIntelligentPoolWindowOption = minQuotesToQualifyForIntelligentPoolWindowOption,
      intelligentPoolPercentage = intelligentPoolPercentage,
      speedNormalizer = speedNormalizer,
      driverSpeedWeightage = driverSpeedWeightage,
      minLocationUpdates = minLocationUpdates,
      locationUpdateSampleTime = locationUpdateSampleTime,
      defaultDriverSpeed = defaultDriverSpeed,
      createdAt = createdAt,
      updatedAt = updatedAt
    }

transformDomainDriverIntelligentPoolConfigToBeam :: DriverIntelligentPoolConfig -> BeamDIPC.DriverIntelligentPoolConfig
transformDomainDriverIntelligentPoolConfigToBeam DriverIntelligentPoolConfig {..} =
  BeamDIPC.DriverIntelligentPoolConfigT
    { BeamDIPC.merchantId = getId merchantId,
      BeamDIPC.availabilityTimeWeightage = availabilityTimeWeightage,
      BeamDIPC.availabilityTimeWindowOption = availabilityTimeWindowOption,
      BeamDIPC.acceptanceRatioWeightage = acceptanceRatioWeightage,
      BeamDIPC.acceptanceRatioWindowOption = acceptanceRatioWindowOption,
      BeamDIPC.cancellationRatioWeightage = cancellationRatioWeightage,
      BeamDIPC.cancellationRatioWindowOption = cancellationRatioWindowOption,
      BeamDIPC.minQuotesToQualifyForIntelligentPool = minQuotesToQualifyForIntelligentPool,
      BeamDIPC.minQuotesToQualifyForIntelligentPoolWindowOption = minQuotesToQualifyForIntelligentPoolWindowOption,
      BeamDIPC.intelligentPoolPercentage = intelligentPoolPercentage,
      BeamDIPC.speedNormalizer = speedNormalizer,
      BeamDIPC.driverSpeedWeightage = driverSpeedWeightage,
      BeamDIPC.minLocationUpdates = minLocationUpdates,
      BeamDIPC.locationUpdateSampleTime = locationUpdateSampleTime,
      BeamDIPC.defaultDriverSpeed = defaultDriverSpeed,
      BeamDIPC.createdAt = createdAt,
      BeamDIPC.updatedAt = updatedAt
    }
