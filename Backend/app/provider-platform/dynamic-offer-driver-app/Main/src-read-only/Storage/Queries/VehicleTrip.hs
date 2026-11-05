{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehicleTrip where

import qualified Domain.Types.VehicleTrip
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.VehicleTrip as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleTrip.VehicleTrip -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.VehicleTrip.VehicleTrip] -> m ())
createMany = traverse_ create

closeTrip ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.VehicleTrip.VehicleTripStatus -> Kernel.Prelude.Maybe Domain.Types.VehicleTrip.VehicleTripEndReason -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.VehicleTrip.VehicleTrip -> m ())
closeTrip status endReason endedAt id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.endReason endReason, Se.Set Beam.endedAt endedAt, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findActiveByVehicleNumber :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.VehicleTrip.VehicleTrip))
findActiveByVehicleNumber vehicleNumber = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.vehicleNumber $ Se.Eq vehicleNumber,
          Se.Is Beam.status $ Se.In [Domain.Types.VehicleTrip.ACTIVE, Domain.Types.VehicleTrip.PAUSED]
        ]
    ]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.VehicleTrip.VehicleTrip -> m (Maybe Domain.Types.VehicleTrip.VehicleTrip))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateOfflineBoardings :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.VehicleTrip.VehicleTrip -> m ())
updateOfflineBoardings offlineBoardings id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.offlineBoardings offlineBoardings, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleTrip.VehicleTripStatus -> Kernel.Types.Id.Id Domain.Types.VehicleTrip.VehicleTrip -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.VehicleTrip.VehicleTrip -> m (Maybe Domain.Types.VehicleTrip.VehicleTrip))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleTrip.VehicleTrip -> m ())
updateByPrimaryKey (Domain.Types.VehicleTrip.VehicleTrip {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.capacity capacity,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.endReason endReason,
      Se.Set Beam.endedAt endedAt,
      Se.Set Beam.integratedBppConfigId (Kernel.Types.Id.getId integratedBppConfigId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.offlineBoardings offlineBoardings,
      Se.Set Beam.routeCode routeCode,
      Se.Set Beam.startedAt startedAt,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleNumber vehicleNumber
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.VehicleTrip Domain.Types.VehicleTrip.VehicleTrip where
  fromTType' (Beam.VehicleTripT {..}) = do
    pure $
      Just
        Domain.Types.VehicleTrip.VehicleTrip
          { capacity = capacity,
            createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            endReason = endReason,
            endedAt = endedAt,
            id = Kernel.Types.Id.Id id,
            integratedBppConfigId = Kernel.Types.Id.Id integratedBppConfigId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            offlineBoardings = offlineBoardings,
            routeCode = routeCode,
            startedAt = startedAt,
            status = status,
            updatedAt = updatedAt,
            vehicleNumber = vehicleNumber
          }

instance ToTType' Beam.VehicleTrip Domain.Types.VehicleTrip.VehicleTrip where
  toTType' (Domain.Types.VehicleTrip.VehicleTrip {..}) = do
    Beam.VehicleTripT
      { Beam.capacity = capacity,
        Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.endReason = endReason,
        Beam.endedAt = endedAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.integratedBppConfigId = Kernel.Types.Id.getId integratedBppConfigId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.offlineBoardings = offlineBoardings,
        Beam.routeCode = routeCode,
        Beam.startedAt = startedAt,
        Beam.status = status,
        Beam.updatedAt = updatedAt,
        Beam.vehicleNumber = vehicleNumber
      }
