{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TripTransaction (module Storage.Queries.TripTransaction, module ReExport) where

import qualified Data.Text
import qualified Domain.Types.Person
import qualified Domain.Types.TripTransaction
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TripTransaction as Beam
import Storage.Queries.TripTransactionExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TripTransaction.TripTransaction -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.TripTransaction.TripTransaction] -> m ())
createMany = traverse_ create

findAllTripTransactionByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.TripTransaction.TripTransaction])
findAllTripTransactionByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByTransactionId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TripTransaction.TripTransaction -> m (Maybe Domain.Types.TripTransaction.TripTransaction))
findByTransactionId id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateOnStart ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Data.Text.Text -> Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong -> Domain.Types.TripTransaction.TripStatus -> Kernel.Types.Id.Id Domain.Types.TripTransaction.TripTransaction -> m ())
updateOnStart routeCode startLocation status id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.routeCode routeCode,
      Se.Set Beam.startLocationLat (Kernel.Prelude.fmap (.lat) startLocation),
      Se.Set Beam.startLocationLon (Kernel.Prelude.fmap (.lon) startLocation),
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.TripTransaction.TripStatus -> Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong -> Kernel.Types.Id.Id Domain.Types.TripTransaction.TripTransaction -> m ())
updateStatus status endLocation id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.endLocationLat (Kernel.Prelude.fmap (.lat) endLocation),
      Se.Set Beam.endLocationLon (Kernel.Prelude.fmap (.lon) endLocation),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.TripTransaction.TripTransaction -> m (Maybe Domain.Types.TripTransaction.TripTransaction))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TripTransaction.TripTransaction -> m ())
updateByPrimaryKey (Domain.Types.TripTransaction.TripTransaction {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.allowEndingMidRoute allowEndingMidRoute,
      Se.Set Beam.deviationCount deviationCount,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.endLocationLat (Kernel.Prelude.fmap (.lat) endLocation),
      Se.Set Beam.endLocationLon (Kernel.Prelude.fmap (.lon) endLocation),
      Se.Set Beam.endStopCode endStopCode,
      Se.Set Beam.fleetOwnerId (Kernel.Types.Id.getId fleetOwnerId),
      Se.Set Beam.isCurrentlyDeviated isCurrentlyDeviated,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.routeCode routeCode,
      Se.Set Beam.sequenceNumber sequenceNumber,
      Se.Set Beam.startLocationLat (Kernel.Prelude.fmap (.lat) startLocation),
      Se.Set Beam.startLocationLon (Kernel.Prelude.fmap (.lon) startLocation),
      Se.Set Beam.startedNearStopCode startedNearStopCode,
      Se.Set Beam.status status,
      Se.Set Beam.tripCode tripCode,
      Se.Set Beam.vehicleNumber vehicleNumber,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]