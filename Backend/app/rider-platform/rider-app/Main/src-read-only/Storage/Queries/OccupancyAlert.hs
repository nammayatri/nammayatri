{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OccupancyAlert where

import qualified Domain.Types.OccupancyAlert
import qualified Domain.Types.VehicleViolation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.OccupancyAlert as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.OccupancyAlert.OccupancyAlert -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.OccupancyAlert.OccupancyAlert] -> m ()
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.OccupancyAlert.OccupancyAlert -> m (Maybe Domain.Types.OccupancyAlert.OccupancyAlert)
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateResolution ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Domain.Types.OccupancyAlert.AlertStatus ->
  (Kernel.Prelude.Maybe Domain.Types.OccupancyAlert.AlertResolution -> (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.VehicleViolation.VehicleViolation) -> (Kernel.Prelude.Maybe Kernel.Prelude.Text -> (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> (Kernel.Types.Id.Id Domain.Types.OccupancyAlert.OccupancyAlert -> m ())))))
updateResolution status resolution violationId acknowledgedBy resolvedAt id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.resolution resolution,
      Se.Set Beam.violationId (Kernel.Types.Id.getId <$> violationId),
      Se.Set Beam.acknowledgedBy acknowledgedBy,
      Se.Set Beam.resolvedAt resolvedAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.OccupancyAlert.OccupancyAlert -> m (Maybe Domain.Types.OccupancyAlert.OccupancyAlert)
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.OccupancyAlert.OccupancyAlert -> m ()
updateByPrimaryKey (Domain.Types.OccupancyAlert.OccupancyAlert {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.acknowledgedBy acknowledgedBy,
      Se.Set Beam.category category,
      Se.Set Beam.detectedAt detectedAt,
      Se.Set Beam.detectionSourceId (Kernel.Types.Id.getId detectionSourceId),
      Se.Set Beam.externalAlertId externalAlertId,
      Se.Set Beam.graceUntil graceUntil,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.resolution resolution,
      Se.Set Beam.resolvedAt resolvedAt,
      Se.Set Beam.status status,
      Se.Set Beam.ticketServiceId (Kernel.Types.Id.getId ticketServiceId),
      Se.Set Beam.unpaidEstimate unpaidEstimate,
      Se.Set Beam.violationId (Kernel.Types.Id.getId <$> violationId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.OccupancyAlert Domain.Types.OccupancyAlert.OccupancyAlert where
  fromTType' (Beam.OccupancyAlertT {..}) = do
    pure $
      Just
        Domain.Types.OccupancyAlert.OccupancyAlert
          { acknowledgedBy = acknowledgedBy,
            category = category,
            detectedAt = detectedAt,
            detectionSourceId = Kernel.Types.Id.Id detectionSourceId,
            externalAlertId = externalAlertId,
            graceUntil = graceUntil,
            id = Kernel.Types.Id.Id id,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            resolution = resolution,
            resolvedAt = resolvedAt,
            status = status,
            ticketServiceId = Kernel.Types.Id.Id ticketServiceId,
            unpaidEstimate = unpaidEstimate,
            violationId = Kernel.Types.Id.Id <$> violationId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.OccupancyAlert Domain.Types.OccupancyAlert.OccupancyAlert where
  toTType' (Domain.Types.OccupancyAlert.OccupancyAlert {..}) = do
    Beam.OccupancyAlertT
      { Beam.acknowledgedBy = acknowledgedBy,
        Beam.category = category,
        Beam.detectedAt = detectedAt,
        Beam.detectionSourceId = Kernel.Types.Id.getId detectionSourceId,
        Beam.externalAlertId = externalAlertId,
        Beam.graceUntil = graceUntil,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.resolution = resolution,
        Beam.resolvedAt = resolvedAt,
        Beam.status = status,
        Beam.ticketServiceId = Kernel.Types.Id.getId ticketServiceId,
        Beam.unpaidEstimate = unpaidEstimate,
        Beam.violationId = Kernel.Types.Id.getId <$> violationId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
