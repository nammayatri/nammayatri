{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PTCircuitBreakerHistory where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PTCircuitBreakerHistory
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PTCircuitBreakerHistory as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PTCircuitBreakerHistory.PTCircuitBreakerHistory -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PTCircuitBreakerHistory.PTCircuitBreakerHistory] -> m ())
createMany = traverse_ create

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.PTCircuitBreakerHistory.PTCircuitBreakerHistory -> m (Maybe Domain.Types.PTCircuitBreakerHistory.PTCircuitBreakerHistory))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ([Domain.Types.PTCircuitBreakerHistory.PTCircuitBreakerHistory]))
findByMerchantOperatingCityId limit offset merchantOperatingCityId = do findAllWithOptionsDb [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]] (Se.Desc Beam.createdAt) limit offset

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.PTCircuitBreakerHistory.PTCircuitBreakerHistory -> m (Maybe Domain.Types.PTCircuitBreakerHistory.PTCircuitBreakerHistory))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PTCircuitBreakerHistory.PTCircuitBreakerHistory -> m ())
updateByPrimaryKey (Domain.Types.PTCircuitBreakerHistory.PTCircuitBreakerHistory {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.apiType apiType,
      Se.Set Beam.failureCount failureCount,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.newState newState,
      Se.Set Beam.previousState previousState,
      Se.Set Beam.ptMode ptMode,
      Se.Set Beam.reason reason,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PTCircuitBreakerHistory Domain.Types.PTCircuitBreakerHistory.PTCircuitBreakerHistory where
  fromTType' (Beam.PTCircuitBreakerHistoryT {..}) = do
    pure $
      Just
        Domain.Types.PTCircuitBreakerHistory.PTCircuitBreakerHistory
          { apiType = apiType,
            createdAt = createdAt,
            failureCount = failureCount,
            id = Kernel.Types.Id.Id id,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            newState = newState,
            previousState = previousState,
            ptMode = ptMode,
            reason = reason,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PTCircuitBreakerHistory Domain.Types.PTCircuitBreakerHistory.PTCircuitBreakerHistory where
  toTType' (Domain.Types.PTCircuitBreakerHistory.PTCircuitBreakerHistory {..}) = do
    Beam.PTCircuitBreakerHistoryT
      { Beam.apiType = apiType,
        Beam.createdAt = createdAt,
        Beam.failureCount = failureCount,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.newState = newState,
        Beam.previousState = previousState,
        Beam.ptMode = ptMode,
        Beam.reason = reason,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.updatedAt = updatedAt
      }
