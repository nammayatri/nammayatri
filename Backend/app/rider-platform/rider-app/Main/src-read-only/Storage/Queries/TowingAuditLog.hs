{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TowingAuditLog where

import qualified Domain.Types.TowingAuditLog
import qualified Domain.Types.TowingRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TowingAuditLog as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TowingAuditLog.TowingAuditLog -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.TowingAuditLog.TowingAuditLog] -> m ())
createMany = traverse_ create

findAllByTowingRequestId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TowingRequest.TowingRequest -> m [Domain.Types.TowingAuditLog.TowingAuditLog])
findAllByTowingRequestId towingRequestId = do findAllWithKV [Se.Is Beam.towingRequestId $ Se.Eq (Kernel.Types.Id.getId towingRequestId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TowingAuditLog.TowingAuditLog -> m (Maybe Domain.Types.TowingAuditLog.TowingAuditLog))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TowingAuditLog.TowingAuditLog -> m ())
updateByPrimaryKey (Domain.Types.TowingAuditLog.TowingAuditLog {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.action action,
      Se.Set Beam.lat lat,
      Se.Set Beam.lon lon,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.newStatus newStatus,
      Se.Set Beam.note note,
      Se.Set Beam.performedBy performedBy,
      Se.Set Beam.performedByName performedByName,
      Se.Set Beam.towingRequestId (Kernel.Types.Id.getId towingRequestId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.TowingAuditLog Domain.Types.TowingAuditLog.TowingAuditLog where
  fromTType' (Beam.TowingAuditLogT {..}) = do
    pure $
      Just
        Domain.Types.TowingAuditLog.TowingAuditLog
          { action = action,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            lon = lon,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            newStatus = newStatus,
            note = note,
            performedBy = performedBy,
            performedByName = performedByName,
            towingRequestId = Kernel.Types.Id.Id towingRequestId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TowingAuditLog Domain.Types.TowingAuditLog.TowingAuditLog where
  toTType' (Domain.Types.TowingAuditLog.TowingAuditLog {..}) = do
    Beam.TowingAuditLogT
      { Beam.action = action,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.newStatus = newStatus,
        Beam.note = note,
        Beam.performedBy = performedBy,
        Beam.performedByName = performedByName,
        Beam.towingRequestId = Kernel.Types.Id.getId towingRequestId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
