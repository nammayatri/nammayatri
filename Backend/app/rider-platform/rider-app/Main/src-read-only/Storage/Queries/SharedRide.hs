{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SharedRide where

import qualified Domain.Types.Person
import qualified Domain.Types.SharedBooking
import qualified Domain.Types.SharedRide
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SharedRide as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedRide.SharedRide -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SharedRide.SharedRide] -> m ())
createMany = traverse_ create

findByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.SharedRide.SharedRide]))
findByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findBySharedBookingId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SharedBooking.SharedBooking -> m (Maybe Domain.Types.SharedRide.SharedRide))
findBySharedBookingId sharedBookingId = do findOneWithKV [Se.Is Beam.sharedBookingId $ Se.Eq (Kernel.Types.Id.getId sharedBookingId)]

findByStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedRide.SharedRideStatus -> m ([Domain.Types.SharedRide.SharedRide]))
findByStatus status = do findAllWithKV [Se.Is Beam.status $ Se.Eq status]

updateRideEndTime :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.SharedRide.SharedRide -> m ())
updateRideEndTime rideEndTime id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.rideEndTime rideEndTime, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateRideStartTime :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.SharedRide.SharedRide -> m ())
updateRideStartTime rideStartTime id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.rideStartTime rideStartTime, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedRide.SharedRideStatus -> Kernel.Types.Id.Id Domain.Types.SharedRide.SharedRide -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateTotalFare :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Id.Id Domain.Types.SharedRide.SharedRide -> m ())
updateTotalFare totalFare id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.totalFare totalFare, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SharedRide.SharedRide -> m (Maybe Domain.Types.SharedRide.SharedRide))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedRide.SharedRide -> m ())
updateByPrimaryKey (Domain.Types.SharedRide.SharedRide {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bppSharedRideId bppSharedRideId,
      Se.Set Beam.chargeableDistanceValue chargeableDistanceValue,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.rideEndTime rideEndTime,
      Se.Set Beam.rideIds (Kernel.Types.Id.getId <$> rideIds),
      Se.Set Beam.rideStartTime rideStartTime,
      Se.Set Beam.sharedBookingId (Kernel.Types.Id.getId sharedBookingId),
      Se.Set Beam.status status,
      Se.Set Beam.totalFare totalFare,
      Se.Set Beam.trackingUrl ((Kernel.Prelude.fmap showBaseUrl) trackingUrl),
      Se.Set Beam.traveledDistanceValue traveledDistanceValue,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleModel vehicleModel,
      Se.Set Beam.vehicleNumber vehicleNumber,
      Se.Set Beam.vehicleServiceTierType vehicleServiceTierType,
      Se.Set Beam.vehicleVariant vehicleVariant,
      Se.Set Beam.waypoints waypoints
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.SharedRide Domain.Types.SharedRide.SharedRide where
  fromTType' (Beam.SharedRideT {..}) = do
    trackingUrl' <- (Kernel.Prelude.mapM parseBaseUrl) trackingUrl
    pure $
      Just
        Domain.Types.SharedRide.SharedRide
          { bppSharedRideId = bppSharedRideId,
            chargeableDistanceValue = chargeableDistanceValue,
            createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            rideEndTime = rideEndTime,
            rideIds = Kernel.Types.Id.Id <$> rideIds,
            rideStartTime = rideStartTime,
            sharedBookingId = Kernel.Types.Id.Id sharedBookingId,
            status = status,
            totalFare = totalFare,
            trackingUrl = trackingUrl',
            traveledDistanceValue = traveledDistanceValue,
            updatedAt = updatedAt,
            vehicleModel = vehicleModel,
            vehicleNumber = vehicleNumber,
            vehicleServiceTierType = vehicleServiceTierType,
            vehicleVariant = vehicleVariant,
            waypoints = waypoints
          }

instance ToTType' Beam.SharedRide Domain.Types.SharedRide.SharedRide where
  toTType' (Domain.Types.SharedRide.SharedRide {..}) = do
    Beam.SharedRideT
      { Beam.bppSharedRideId = bppSharedRideId,
        Beam.chargeableDistanceValue = chargeableDistanceValue,
        Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.rideEndTime = rideEndTime,
        Beam.rideIds = Kernel.Types.Id.getId <$> rideIds,
        Beam.rideStartTime = rideStartTime,
        Beam.sharedBookingId = Kernel.Types.Id.getId sharedBookingId,
        Beam.status = status,
        Beam.totalFare = totalFare,
        Beam.trackingUrl = (Kernel.Prelude.fmap showBaseUrl) trackingUrl,
        Beam.traveledDistanceValue = traveledDistanceValue,
        Beam.updatedAt = updatedAt,
        Beam.vehicleModel = vehicleModel,
        Beam.vehicleNumber = vehicleNumber,
        Beam.vehicleServiceTierType = vehicleServiceTierType,
        Beam.vehicleVariant = vehicleVariant,
        Beam.waypoints = waypoints
      }
