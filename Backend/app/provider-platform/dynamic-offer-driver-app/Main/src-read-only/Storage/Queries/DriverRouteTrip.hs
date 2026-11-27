{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverRouteTrip where

import qualified Domain.Types.DriverRouteTrip
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverRouteTrip as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.DriverRouteTrip.DriverRouteTrip -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.DriverRouteTrip.DriverRouteTrip] -> m ()
createMany = traverse_ create

findAllByDriverIdAndCityCreatedSince ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Maybe Int ->
  (Maybe Int -> (Kernel.Types.Id.Id Domain.Types.Person.Person -> (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> (Kernel.Prelude.UTCTime -> m [Domain.Types.DriverRouteTrip.DriverRouteTrip]))))
findAllByDriverIdAndCityCreatedSince limit offset driverId merchantOperatingCityId since = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.createdAt $ Se.GreaterThanOrEq since
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findByIdempotencyKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Prelude.Text -> m (Maybe Domain.Types.DriverRouteTrip.DriverRouteTrip)
findByIdempotencyKey idempotencyKey = do findOneWithKV [Se.Is Beam.idempotencyKey $ Se.Eq idempotencyKey]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Types.Id.Id Domain.Types.DriverRouteTrip.DriverRouteTrip -> m (Maybe Domain.Types.DriverRouteTrip.DriverRouteTrip)
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.DriverRouteTrip.DriverRouteTrip -> m ()
updateByPrimaryKey (Domain.Types.DriverRouteTrip.DriverRouteTrip {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.idempotencyKey idempotencyKey,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.rideId rideId,
      Se.Set Beam.routeCode routeCode,
      Se.Set Beam.source source,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleNumber vehicleNumber
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.DriverRouteTrip Domain.Types.DriverRouteTrip.DriverRouteTrip where
  fromTType' (Beam.DriverRouteTripT {..}) = do
    pure $
      Just
        Domain.Types.DriverRouteTrip.DriverRouteTrip
          { createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            idempotencyKey = idempotencyKey,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            rideId = rideId,
            routeCode = routeCode,
            source = source,
            updatedAt = updatedAt,
            vehicleNumber = vehicleNumber
          }

instance ToTType' Beam.DriverRouteTrip Domain.Types.DriverRouteTrip.DriverRouteTrip where
  toTType' (Domain.Types.DriverRouteTrip.DriverRouteTrip {..}) = do
    Beam.DriverRouteTripT
      { Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.idempotencyKey = idempotencyKey,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.rideId = rideId,
        Beam.routeCode = routeCode,
        Beam.source = source,
        Beam.updatedAt = updatedAt,
        Beam.vehicleNumber = vehicleNumber
      }
