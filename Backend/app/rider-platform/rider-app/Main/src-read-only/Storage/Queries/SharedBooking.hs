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
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.SharedBooking as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedBooking.SharedBooking -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.SharedBooking.SharedBooking] -> m ())
createMany = traverse_ create

findBySharedEstimateId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SharedEstimate.SharedEstimate -> m (Maybe Domain.Types.SharedBooking.SharedBooking))
findBySharedEstimateId sharedEstimateId = do findOneWithKV [Se.Is Beam.sharedEstimateId $ Se.Eq (Kernel.Types.Id.getId sharedEstimateId)]

findByStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.SharedBooking.BookingStatus -> m [Domain.Types.SharedBooking.SharedBooking])
findByStatus status = do findAllWithKV [Se.Is Beam.status $ Se.Eq status]

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
      Se.Set Beam.bppSharedBookingId bppSharedBookingId,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.distanceUnit (Kernel.Prelude.Just distanceUnit),
      Se.Set Beam.driverId (Kernel.Types.Id.getId <$> driverId),
      Se.Set Beam.estimatedDistance (Kernel.Types.Common.distanceToHighPrecMeters <$> estimatedDistance),
      Se.Set Beam.estimatedDistanceValue (Kernel.Utils.Common.distanceToHighPrecDistance distanceUnit <$> estimatedDistance),
      Se.Set Beam.estimatedDuration estimatedDuration,
      Se.Set Beam.currency (Just $ (.currency) estimatedTotalFare),
      Se.Set Beam.estimatedTotalFare ((.amount) estimatedTotalFare),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.pairingTime pairingTime,
      Se.Set Beam.providerId providerId,
      Se.Set Beam.providerUrl (Kernel.Prelude.showBaseUrl providerUrl),
      Se.Set Beam.sharedEstimateId (Kernel.Types.Id.getId sharedEstimateId),
      Se.Set Beam.status status,
      Se.Set Beam.transactionId transactionId,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.vehicleServiceTierType vehicleServiceTierType
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.SharedBooking Domain.Types.SharedBooking.SharedBooking where
  fromTType' (Beam.SharedBookingT {..}) = do
    providerUrl' <- Kernel.Prelude.parseBaseUrl providerUrl
    pure $
      Just
        Domain.Types.SharedBooking.SharedBooking
          { bookingIds = Kernel.Types.Id.Id <$> bookingIds,
            bppSharedBookingId = bppSharedBookingId,
            createdAt = createdAt,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            driverId = Kernel.Types.Id.Id <$> driverId,
            estimatedDistance = Kernel.Utils.Common.mkDistanceWithDefault distanceUnit estimatedDistanceValue <$> estimatedDistance,
            estimatedDuration = estimatedDuration,
            estimatedTotalFare = Kernel.Types.Common.mkPrice currency estimatedTotalFare,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            pairingTime = pairingTime,
            providerId = providerId,
            providerUrl = providerUrl',
            sharedEstimateId = Kernel.Types.Id.Id sharedEstimateId,
            status = status,
            transactionId = transactionId,
            updatedAt = updatedAt,
            vehicleServiceTierType = vehicleServiceTierType
          }

instance ToTType' Beam.SharedBooking Domain.Types.SharedBooking.SharedBooking where
  toTType' (Domain.Types.SharedBooking.SharedBooking {..}) = do
    Beam.SharedBookingT
      { Beam.bookingIds = Kernel.Types.Id.getId <$> bookingIds,
        Beam.bppSharedBookingId = bppSharedBookingId,
        Beam.createdAt = createdAt,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.driverId = Kernel.Types.Id.getId <$> driverId,
        Beam.estimatedDistance = Kernel.Types.Common.distanceToHighPrecMeters <$> estimatedDistance,
        Beam.estimatedDistanceValue = Kernel.Utils.Common.distanceToHighPrecDistance distanceUnit <$> estimatedDistance,
        Beam.estimatedDuration = estimatedDuration,
        Beam.currency = Just $ (.currency) estimatedTotalFare,
        Beam.estimatedTotalFare = (.amount) estimatedTotalFare,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.pairingTime = pairingTime,
        Beam.providerId = providerId,
        Beam.providerUrl = Kernel.Prelude.showBaseUrl providerUrl,
        Beam.sharedEstimateId = Kernel.Types.Id.getId sharedEstimateId,
        Beam.status = status,
        Beam.transactionId = transactionId,
        Beam.updatedAt = updatedAt,
        Beam.vehicleServiceTierType = vehicleServiceTierType
      }
