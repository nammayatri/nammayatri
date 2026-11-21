{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehicleViolation where

import qualified Domain.Types.VehicleViolation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VehicleViolation as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleViolation.VehicleViolation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VehicleViolation.VehicleViolation] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.VehicleViolation.VehicleViolation -> m (Maybe Domain.Types.VehicleViolation.VehicleViolation))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateApproval ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.VehicleViolation.ViolationStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.VehicleViolation.VehicleViolation -> m ())
updateApproval status approvedBy id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.approvedBy approvedBy, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.VehicleViolation.VehicleViolation -> m (Maybe Domain.Types.VehicleViolation.VehicleViolation))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleViolation.VehicleViolation -> m ())
updateByPrimaryKey (Domain.Types.VehicleViolation.VehicleViolation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.approvedBy approvedBy,
      Se.Set Beam.currency currency,
      Se.Set Beam.fineAmount fineAmount,
      Se.Set Beam.lat lat,
      Se.Set Beam.lon lon,
      Se.Set Beam.mediaFileIds (Kernel.Types.Id.getId <$> mediaFileIds),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.reportedBy reportedBy,
      Se.Set Beam.reporterPersonId (Kernel.Types.Id.getId <$> reporterPersonId),
      Se.Set Beam.status status,
      Se.Set Beam.ticketPlaceId (Kernel.Types.Id.getId <$> ticketPlaceId),
      Se.Set Beam.vehicleNumber vehicleNumber,
      Se.Set Beam.violationType violationType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.VehicleViolation Domain.Types.VehicleViolation.VehicleViolation where
  fromTType' (Beam.VehicleViolationT {..}) = do
    pure $
      Just
        Domain.Types.VehicleViolation.VehicleViolation
          { approvedBy = approvedBy,
            currency = currency,
            fineAmount = fineAmount,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            lon = lon,
            mediaFileIds = Kernel.Types.Id.Id <$> mediaFileIds,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            reportedBy = reportedBy,
            reporterPersonId = Kernel.Types.Id.Id <$> reporterPersonId,
            status = status,
            ticketPlaceId = Kernel.Types.Id.Id <$> ticketPlaceId,
            vehicleNumber = vehicleNumber,
            violationType = violationType,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.VehicleViolation Domain.Types.VehicleViolation.VehicleViolation where
  toTType' (Domain.Types.VehicleViolation.VehicleViolation {..}) = do
    Beam.VehicleViolationT
      { Beam.approvedBy = approvedBy,
        Beam.currency = currency,
        Beam.fineAmount = fineAmount,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.mediaFileIds = Kernel.Types.Id.getId <$> mediaFileIds,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.reportedBy = reportedBy,
        Beam.reporterPersonId = Kernel.Types.Id.getId <$> reporterPersonId,
        Beam.status = status,
        Beam.ticketPlaceId = Kernel.Types.Id.getId <$> ticketPlaceId,
        Beam.vehicleNumber = vehicleNumber,
        Beam.violationType = violationType,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
