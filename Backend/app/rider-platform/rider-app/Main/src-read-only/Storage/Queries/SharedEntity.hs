{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SharedEntity (module Storage.Queries.SharedEntity, module ReExport) where

import qualified Data.Aeson
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.SharedEntity
import qualified Domain.Types.TrackedEntity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SharedEntity as Beam
import Storage.Queries.SharedEntityExtra as ReExport

findByCounterAppSharedEntityId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Domain.Types.SharedEntity.SharedEntity))
findByCounterAppSharedEntityId counterAppSharedEntityId = do findOneWithKV [Se.Is Beam.counterAppSharedEntityId $ Se.Eq counterAppSharedEntityId]

findByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> m [Domain.Types.SharedEntity.SharedEntity])
findByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId <$> driverId)]

findByMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.SharedEntity.SharedEntity])
findByMerchantOperatingCityId merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findByPooledUsingCustomer :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> m [Domain.Types.SharedEntity.SharedEntity])
findByPooledUsingCustomer pooledUsingCustomer = do findAllWithKV [Se.Is Beam.pooledUsingCustomer $ Se.Eq (Kernel.Types.Id.getId <$> pooledUsingCustomer)]

findByStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedEntity.SharedEntityStatus -> m [Domain.Types.SharedEntity.SharedEntity])
findByStatus status = do findAllWithKV [Se.Is Beam.status $ Se.Eq status]

findByTransactionId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Domain.Types.SharedEntity.SharedEntity))
findByTransactionId transactionId = do findOneWithKV [Se.Is Beam.transactionId $ Se.Eq transactionId]

updateBookingIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe [Domain.Types.TrackedEntity.TrackedEntity] -> Kernel.Types.Id.Id Domain.Types.SharedEntity.SharedEntity -> m ())
updateBookingIds bookingIds id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.bookingIds (Data.Aeson.toJSON <$> bookingIds), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateCounterAppEntityId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Domain.Types.SharedEntity.SharedEntity -> m ())
updateCounterAppEntityId counterAppSharedEntityId id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.counterAppSharedEntityId counterAppSharedEntityId, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateDriverAssignment ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Types.Id.Id Domain.Types.SharedEntity.SharedEntity -> m ())
updateDriverAssignment driverId id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.driverId (Kernel.Types.Id.getId <$> driverId), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateEstimateIds ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe [Domain.Types.TrackedEntity.TrackedEntity] -> Kernel.Types.Id.Id Domain.Types.SharedEntity.SharedEntity -> m ())
updateEstimateIds estimateIds id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.estimateIds (Data.Aeson.toJSON <$> estimateIds), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updatePooledUsingCustomer ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Types.Id.Id Domain.Types.SharedEntity.SharedEntity -> m ())
updatePooledUsingCustomer pooledUsingCustomer id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.pooledUsingCustomer (Kernel.Types.Id.getId <$> pooledUsingCustomer), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateRideIds :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe [Domain.Types.TrackedEntity.TrackedEntity] -> Kernel.Types.Id.Id Domain.Types.SharedEntity.SharedEntity -> m ())
updateRideIds rideIds id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.rideIds (Data.Aeson.toJSON <$> rideIds), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedEntity.SharedEntityStatus -> Kernel.Types.Id.Id Domain.Types.SharedEntity.SharedEntity -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SharedEntity.SharedEntity -> m (Maybe Domain.Types.SharedEntity.SharedEntity))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedEntity.SharedEntity -> m ())
updateByPrimaryKey (Domain.Types.SharedEntity.SharedEntity {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bookingIds (Data.Aeson.toJSON <$> bookingIds),
      Se.Set Beam.counterAppSharedEntityId counterAppSharedEntityId,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.driverId (Kernel.Types.Id.getId <$> driverId),
      Se.Set Beam.entityType entityType,
      Se.Set Beam.estimateIds (Data.Aeson.toJSON <$> estimateIds),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.pairingTime pairingTime,
      Se.Set Beam.pooledUsingCustomer (Kernel.Types.Id.getId <$> pooledUsingCustomer),
      Se.Set Beam.rideIds (Data.Aeson.toJSON <$> rideIds),
      Se.Set Beam.searchRequestIds (Data.Aeson.toJSON <$> searchRequestIds),
      Se.Set Beam.status status,
      Se.Set Beam.totalSeats totalSeats,
      Se.Set Beam.transactionId transactionId,
      Se.Set Beam.tripCategory tripCategory,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.validTill validTill,
      Se.Set Beam.vehicleCategory vehicleCategory,
      Se.Set Beam.waypoints waypoints
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
