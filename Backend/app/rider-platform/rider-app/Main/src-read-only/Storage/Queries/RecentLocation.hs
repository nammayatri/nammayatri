{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RecentLocation (module Storage.Queries.RecentLocation, module ReExport) where

import qualified Domain.Types.Person
import qualified Domain.Types.RecentLocation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RecentLocation as Beam
import Storage.Queries.RecentLocationExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RecentLocation.RecentLocation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RecentLocation.RecentLocation] -> m ())
createMany = traverse_ create

findAllRecentLocationsForPerson :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.RecentLocation.RecentLocation])
findAllRecentLocationsForPerson riderId = do findAllWithDb [Se.Is Beam.riderId $ Se.Eq (Kernel.Types.Id.getId riderId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RecentLocation.RecentLocation -> m (Maybe Domain.Types.RecentLocation.RecentLocation))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RecentLocation.RecentLocation -> m ())
updateByPrimaryKey (Domain.Types.RecentLocation.RecentLocation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.address address,
      Se.Set Beam.entityType entityType,
      Se.Set Beam.fare fare,
      Se.Set Beam.frequency frequency,
      Se.Set Beam.fromGeohash fromGeohash,
      Se.Set Beam.stopLat (fromLatLong <&> (.lat)),
      Se.Set Beam.stopLon (fromLatLong <&> (.lon)),
      Se.Set Beam.fromStopCode fromStopCode,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.routeCode routeCode,
      Se.Set Beam.toGeohash toGeohash,
      Se.Set Beam.lat ((.lat) toLatLong),
      Se.Set Beam.lon ((.lon) toLatLong),
      Se.Set Beam.stopCode toStopCode,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
