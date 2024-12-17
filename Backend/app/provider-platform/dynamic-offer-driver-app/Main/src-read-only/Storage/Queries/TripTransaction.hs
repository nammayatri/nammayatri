{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TripTransaction where

import qualified Domain.Types.TripTransaction
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TripTransaction as Beam
import qualified Storage.Queries.Transformers.Ride

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TripTransaction.TripTransaction -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.TripTransaction.TripTransaction] -> m ())
createMany = traverse_ create

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
      Se.Set Beam.startLocationLat (Kernel.Prelude.fmap (.lat) startLocation),
      Se.Set Beam.startLocationLon (Kernel.Prelude.fmap (.lon) startLocation),
      Se.Set Beam.startedNearStopCode startedNearStopCode,
      Se.Set Beam.status status,
      Se.Set Beam.tripCode tripCode,
      Se.Set Beam.vehicleNumber vehicleNumber,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.TripTransaction Domain.Types.TripTransaction.TripTransaction where
  fromTType' (Beam.TripTransactionT {..}) = do
    pure $
      Just
        Domain.Types.TripTransaction.TripTransaction
          { allowEndingMidRoute = allowEndingMidRoute,
            deviationCount = deviationCount,
            driverId = Kernel.Types.Id.Id driverId,
            endLocation = Storage.Queries.Transformers.Ride.mkLatLong endLocationLat endLocationLon,
            endStopCode = endStopCode,
            fleetOwnerId = Kernel.Types.Id.Id fleetOwnerId,
            id = Kernel.Types.Id.Id id,
            isCurrentlyDeviated = isCurrentlyDeviated,
            startLocation = Storage.Queries.Transformers.Ride.mkLatLong startLocationLat startLocationLon,
            startedNearStopCode = startedNearStopCode,
            status = status,
            tripCode = tripCode,
            vehicleNumber = vehicleNumber,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TripTransaction Domain.Types.TripTransaction.TripTransaction where
  toTType' (Domain.Types.TripTransaction.TripTransaction {..}) = do
    Beam.TripTransactionT
      { Beam.allowEndingMidRoute = allowEndingMidRoute,
        Beam.deviationCount = deviationCount,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.endLocationLat = Kernel.Prelude.fmap (.lat) endLocation,
        Beam.endLocationLon = Kernel.Prelude.fmap (.lon) endLocation,
        Beam.endStopCode = endStopCode,
        Beam.fleetOwnerId = Kernel.Types.Id.getId fleetOwnerId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isCurrentlyDeviated = isCurrentlyDeviated,
        Beam.startLocationLat = Kernel.Prelude.fmap (.lat) startLocation,
        Beam.startLocationLon = Kernel.Prelude.fmap (.lon) startLocation,
        Beam.startedNearStopCode = startedNearStopCode,
        Beam.status = status,
        Beam.tripCode = tripCode,
        Beam.vehicleNumber = vehicleNumber,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
