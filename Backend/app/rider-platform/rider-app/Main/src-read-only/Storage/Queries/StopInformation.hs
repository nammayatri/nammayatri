{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.StopInformation where

import qualified Domain.Types.Ride
import qualified Domain.Types.StopInformation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.StopInformation as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.StopInformation.StopInformation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.StopInformation.StopInformation] -> m ())
createMany = traverse_ create

findAllByRideId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Ride.Ride -> m [Domain.Types.StopInformation.StopInformation])
findAllByRideId rideId = do findAllWithKVAndConditionalDB [Se.Is Beam.rideId $ Se.Eq (Kernel.Types.Id.getId rideId)] Nothing

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.StopInformation.StopInformation -> m (Maybe Domain.Types.StopInformation.StopInformation))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateByStopOrderAndRideId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Ride.Ride -> Kernel.Prelude.Int -> m ())
updateByStopOrderAndRideId waitingTimeEnd rideId stopOrder = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.waitingTimeEnd waitingTimeEnd, Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.rideId $ Se.Eq (Kernel.Types.Id.getId rideId), Se.Is Beam.stopOrder $ Se.Eq stopOrder]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.StopInformation.StopInformation -> m (Maybe Domain.Types.StopInformation.StopInformation))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.StopInformation.StopInformation -> m ())
updateByPrimaryKey (Domain.Types.StopInformation.StopInformation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.rideId (Kernel.Types.Id.getId rideId),
      Se.Set Beam.stopId (Kernel.Types.Id.getId stopId),
      Se.Set Beam.stopOrder stopOrder,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.waitingTimeEnd waitingTimeEnd,
      Se.Set Beam.waitingTimeStart waitingTimeStart,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.StopInformation Domain.Types.StopInformation.StopInformation where
  fromTType' (Beam.StopInformationT {..}) = do
    pure $
      Just
        Domain.Types.StopInformation.StopInformation
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            rideId = Kernel.Types.Id.Id rideId,
            stopId = Kernel.Types.Id.Id stopId,
            stopOrder = stopOrder,
            updatedAt = updatedAt,
            waitingTimeEnd = waitingTimeEnd,
            waitingTimeStart = waitingTimeStart,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.StopInformation Domain.Types.StopInformation.StopInformation where
  toTType' (Domain.Types.StopInformation.StopInformation {..}) = do
    Beam.StopInformationT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.rideId = Kernel.Types.Id.getId rideId,
        Beam.stopId = Kernel.Types.Id.getId stopId,
        Beam.stopOrder = stopOrder,
        Beam.updatedAt = updatedAt,
        Beam.waitingTimeEnd = waitingTimeEnd,
        Beam.waitingTimeStart = waitingTimeStart,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
