{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SavedTrip (module Storage.Queries.SavedTrip, module ReExport) where

import qualified Domain.Types.Person
import qualified Domain.Types.SavedTrip
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SavedTrip as Beam
import Storage.Queries.OrphanInstances.SavedTrip
import Storage.Queries.SavedTripExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SavedTrip.SavedTrip -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SavedTrip.SavedTrip] -> m ())
createMany = traverse_ create

findAllByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.SavedTrip.SavedTrip])
findAllByRiderId riderId = do findAllWithKV [Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SavedTrip.SavedTrip -> m (Maybe Domain.Types.SavedTrip.SavedTrip))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateIsActive :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.SavedTrip.SavedTrip -> m ())
updateIsActive isActive id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.isActive isActive, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateLastNotified ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.SavedTrip.SavedTrip -> m ())
updateLastNotified lastNotifiedAt lastComputedDeparture id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.lastNotifiedAt lastNotifiedAt,
      Se.Set Beam.lastComputedDeparture lastComputedDeparture,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SavedTrip.SavedTrip -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SavedTrip.SavedTrip -> m (Maybe Domain.Types.SavedTrip.SavedTrip))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SavedTrip.SavedTrip -> m ())
updateByPrimaryKey (Domain.Types.SavedTrip.SavedTrip {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.name name,
      Se.Set Beam.originLat originLat,
      Se.Set Beam.originLon originLon,
      Se.Set Beam.originAddress originAddress,
      Se.Set Beam.destinationLat destinationLat,
      Se.Set Beam.destinationLon destinationLon,
      Se.Set Beam.destinationAddress destinationAddress,
      Se.Set Beam.timeMode timeMode,
      Se.Set Beam.targetTime targetTime,
      Se.Set Beam.targetTimeOfDaySeconds (Kernel.Prelude.fmap timeOfDayToSeconds targetTimeOfDay),
      Se.Set Beam.bufferMinutes bufferMinutes,
      Se.Set Beam.recurrence recurrence,
      Se.Set Beam.customDays customDays,
      Se.Set Beam.notifyBeforeMinutes notifyBeforeMinutes,
      Se.Set Beam.isActive isActive,
      Se.Set Beam.lastComputedDeparture lastComputedDeparture,
      Se.Set Beam.lastNotifiedAt lastNotifiedAt,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
