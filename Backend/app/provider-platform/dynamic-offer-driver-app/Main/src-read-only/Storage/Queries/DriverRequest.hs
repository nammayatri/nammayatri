{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverRequest where

import qualified Domain.Types.DriverRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverRequest as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverRequest.DriverRequest -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverRequest.DriverRequest] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverRequest.DriverRequest -> m (Maybe Domain.Types.DriverRequest.DriverRequest))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverRequest.DriverRequest -> m ())
updateByPrimaryKey (Domain.Types.DriverRequest.DriverRequest {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.description description,
      Se.Set Beam.reason reason,
      Se.Set Beam.requestType requestType,
      Se.Set Beam.status status,
      Se.Set Beam.tripTransactionId (Kernel.Types.Id.getId tripTransactionId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.DriverRequest Domain.Types.DriverRequest.DriverRequest where
  fromTType' (Beam.DriverRequestT {..}) = do
    pure $
      Just
        Domain.Types.DriverRequest.DriverRequest
          { description = description,
            id = Kernel.Types.Id.Id id,
            reason = reason,
            requestType = requestType,
            status = status,
            tripTransactionId = Kernel.Types.Id.Id tripTransactionId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DriverRequest Domain.Types.DriverRequest.DriverRequest where
  toTType' (Domain.Types.DriverRequest.DriverRequest {..}) = do
    Beam.DriverRequestT
      { Beam.description = description,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.reason = reason,
        Beam.requestType = requestType,
        Beam.status = status,
        Beam.tripTransactionId = Kernel.Types.Id.getId tripTransactionId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
