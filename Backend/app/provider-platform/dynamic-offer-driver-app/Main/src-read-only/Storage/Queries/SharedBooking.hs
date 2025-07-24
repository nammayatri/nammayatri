{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.SharedBooking where

import qualified Domain.Types.Person
import qualified Domain.Types.SharedBooking
import qualified Domain.Types.SharedEstimate
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.SharedBooking as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedBooking.SharedBooking -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SharedBooking.SharedBooking] -> m ())
createMany = traverse_ create

findByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> m ([Domain.Types.SharedBooking.SharedBooking]))
findByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId <$> driverId)]

findBySharedEstimateId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SharedEstimate.SharedEstimate -> m (Maybe Domain.Types.SharedBooking.SharedBooking))
findBySharedEstimateId sharedEstimateId = do findOneWithKV [Se.Is Beam.sharedEstimateId $ Se.Eq (Kernel.Types.Id.getId sharedEstimateId)]

findByStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedBooking.BookingStatus -> m ([Domain.Types.SharedBooking.SharedBooking]))
findByStatus status = do findAllWithKV [Se.Is Beam.status $ Se.Eq status]

findByTransactionId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.SharedBooking.SharedBooking))
findByTransactionId transactionId = do findOneWithKV [Se.Is Beam.transactionId $ Se.Eq transactionId]

updateDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Kernel.Types.Id.Id Domain.Types.SharedBooking.SharedBooking -> m ())
updateDriverId driverId id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.driverId (Kernel.Types.Id.getId <$> driverId), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedBooking.BookingStatus -> Kernel.Types.Id.Id Domain.Types.SharedBooking.SharedBooking -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SharedBooking.SharedBooking -> m (Maybe Domain.Types.SharedBooking.SharedBooking))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedBooking.SharedBooking -> m ())
updateByPrimaryKey (Domain.Types.SharedBooking.SharedBooking {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bookingIds (Kernel.Types.Id.getId <$> bookingIds),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.distanceUnit distanceUnit,
      Se.Set Beam.driverId (Kernel.Types.Id.getId <$> driverId),
      Se.Set Beam.estimatedDistance estimatedDistance,
      Se.Set Beam.estimatedDuration estimatedDuration,
      Se.Set Beam.estimatedTotalFare estimatedTotalFare,
      Se.Set Beam.fromLocationIds (Kernel.Types.Id.getId <$> fromLocationIds),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.pairingTime pairingTime,
      Se.Set Beam.providerId providerId,
      Se.Set Beam.providerUrl (Kernel.Prelude.showBaseUrl providerUrl),
      Se.Set Beam.sharedEstimateId (Kernel.Types.Id.getId sharedEstimateId),
      Se.Set Beam.status status,
      Se.Set Beam.toLocationIds (Kernel.Types.Id.getId <$> toLocationIds),
      Se.Set Beam.tollNames tollNames,
      Se.Set Beam.transactionId transactionId,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleServiceTier vehicleServiceTier
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.SharedBooking Domain.Types.SharedBooking.SharedBooking where
  fromTType' (Beam.SharedBookingT {..}) = do
    providerUrl' <- Kernel.Prelude.parseBaseUrl providerUrl
    pure $
      Just
        Domain.Types.SharedBooking.SharedBooking
          { bookingIds = Kernel.Types.Id.Id <$> bookingIds,
            createdAt = createdAt,
            distanceUnit = distanceUnit,
            driverId = Kernel.Types.Id.Id <$> driverId,
            estimatedDistance = estimatedDistance,
            estimatedDuration = estimatedDuration,
            estimatedTotalFare = estimatedTotalFare,
            fromLocationIds = Kernel.Types.Id.Id <$> fromLocationIds,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            pairingTime = pairingTime,
            providerId = providerId,
            providerUrl = providerUrl',
            sharedEstimateId = Kernel.Types.Id.Id sharedEstimateId,
            status = status,
            toLocationIds = Kernel.Types.Id.Id <$> toLocationIds,
            tollNames = tollNames,
            transactionId = transactionId,
            updatedAt = updatedAt,
            vehicleServiceTier = vehicleServiceTier
          }

instance ToTType' Beam.SharedBooking Domain.Types.SharedBooking.SharedBooking where
  toTType' (Domain.Types.SharedBooking.SharedBooking {..}) = do
    Beam.SharedBookingT
      { Beam.bookingIds = Kernel.Types.Id.getId <$> bookingIds,
        Beam.createdAt = createdAt,
        Beam.distanceUnit = distanceUnit,
        Beam.driverId = Kernel.Types.Id.getId <$> driverId,
        Beam.estimatedDistance = estimatedDistance,
        Beam.estimatedDuration = estimatedDuration,
        Beam.estimatedTotalFare = estimatedTotalFare,
        Beam.fromLocationIds = Kernel.Types.Id.getId <$> fromLocationIds,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.pairingTime = pairingTime,
        Beam.providerId = providerId,
        Beam.providerUrl = Kernel.Prelude.showBaseUrl providerUrl,
        Beam.sharedEstimateId = Kernel.Types.Id.getId sharedEstimateId,
        Beam.status = status,
        Beam.toLocationIds = Kernel.Types.Id.getId <$> toLocationIds,
        Beam.tollNames = tollNames,
        Beam.transactionId = transactionId,
        Beam.updatedAt = updatedAt,
        Beam.vehicleServiceTier = vehicleServiceTier
      }
