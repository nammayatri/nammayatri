{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.VehicleTrip where

import qualified Domain.Types.Person
import qualified Domain.Types.VehicleTrip
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
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
findActiveByVehicleNumber vehicleNumber = do findOneWithKV [Se.And [Se.Is Beam.vehicleNumber $ Se.Eq vehicleNumber, Se.Is Beam.status $ Se.Eq Domain.Types.VehicleTrip.ACTIVE]]

findAllByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.VehicleTrip.VehicleTrip])
findAllByDriverId limit offset driverId = do findAllWithOptionsKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)] (Se.Desc Beam.startedAt) limit offset

findAllByDriverIdStartedBetween ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> m [Domain.Types.VehicleTrip.VehicleTrip])
findAllByDriverIdStartedBetween limit offset driverId from to = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.startedAt $ Se.GreaterThanOrEq from,
          Se.Is Beam.startedAt $ Se.LessThan to
        ]
    ]
    (Se.Desc Beam.startedAt)
    limit
    offset

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.VehicleTrip.VehicleTrip -> m (Maybe Domain.Types.VehicleTrip.VehicleTrip))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateProgress ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.VehicleTrip.VehicleTrip -> m ())
updateProgress walkupCount earnings id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.walkupCount walkupCount, Se.Set Beam.earnings earnings, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.VehicleTrip.VehicleTrip -> m (Maybe Domain.Types.VehicleTrip.VehicleTrip))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.VehicleTrip.VehicleTrip -> m ())
updateByPrimaryKey (Domain.Types.VehicleTrip.VehicleTrip {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.capacity capacity,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.earnings earnings,
      Se.Set Beam.endReason endReason,
      Se.Set Beam.endedAt endedAt,
      Se.Set Beam.fromStop fromStop,
      Se.Set Beam.integratedBppConfigId (Kernel.Types.Id.getId integratedBppConfigId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.routeCode routeCode,
      Se.Set Beam.startedAt startedAt,
      Se.Set Beam.status status,
      Se.Set Beam.toStop toStop,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleNumber vehicleNumber,
      Se.Set Beam.walkupCount walkupCount
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
            earnings = earnings,
            endReason = endReason,
            endedAt = endedAt,
            fromStop = fromStop,
            id = Kernel.Types.Id.Id id,
            integratedBppConfigId = Kernel.Types.Id.Id integratedBppConfigId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            routeCode = routeCode,
            startedAt = startedAt,
            status = status,
            toStop = toStop,
            updatedAt = updatedAt,
            vehicleNumber = vehicleNumber,
            walkupCount = walkupCount
          }

instance ToTType' Beam.VehicleTrip Domain.Types.VehicleTrip.VehicleTrip where
  toTType' (Domain.Types.VehicleTrip.VehicleTrip {..}) = do
    Beam.VehicleTripT
      { Beam.capacity = capacity,
        Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.earnings = earnings,
        Beam.endReason = endReason,
        Beam.endedAt = endedAt,
        Beam.fromStop = fromStop,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.integratedBppConfigId = Kernel.Types.Id.getId integratedBppConfigId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.routeCode = routeCode,
        Beam.startedAt = startedAt,
        Beam.status = status,
        Beam.toStop = toStop,
        Beam.updatedAt = updatedAt,
        Beam.vehicleNumber = vehicleNumber,
        Beam.walkupCount = walkupCount
      }
