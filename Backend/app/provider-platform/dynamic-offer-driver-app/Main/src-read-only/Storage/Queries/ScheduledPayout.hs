{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ScheduledPayout where

import qualified Domain.Types.ScheduledPayout
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.ScheduledPayout as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ScheduledPayout.ScheduledPayout -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.ScheduledPayout.ScheduledPayout] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.ScheduledPayout.ScheduledPayout -> m (Maybe Domain.Types.ScheduledPayout.ScheduledPayout))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByRideId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.ScheduledPayout.ScheduledPayout))
findByRideId rideId = do findOneWithKV [Se.Is Beam.rideId $ Se.Eq rideId]

incrementRetryCountByRideId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Text -> m ())
incrementRetryCountByRideId retryCount rideId = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.retryCount retryCount, Se.Set Beam.updatedAt _now] [Se.Is Beam.rideId $ Se.Eq rideId]

updateStatusById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ScheduledPayout.ScheduledPayoutStatus -> Kernel.Types.Id.Id Domain.Types.ScheduledPayout.ScheduledPayout -> m ())
updateStatusById status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatusWithReasonByRideId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.ScheduledPayout.ScheduledPayoutStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ())
updateStatusWithReasonByRideId status failureReason rideId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.failureReason failureReason, Se.Set Beam.updatedAt _now] [Se.Is Beam.rideId $ Se.Eq rideId]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.ScheduledPayout.ScheduledPayout -> m (Maybe Domain.Types.ScheduledPayout.ScheduledPayout))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ScheduledPayout.ScheduledPayout -> m ())
updateByPrimaryKey (Domain.Types.ScheduledPayout.ScheduledPayout {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.bookingId bookingId,
      Se.Set Beam.driverId driverId,
      Se.Set Beam.expectedCreditTime expectedCreditTime,
      Se.Set Beam.failureReason failureReason,
      Se.Set Beam.payoutTransactionId payoutTransactionId,
      Se.Set Beam.retryCount retryCount,
      Se.Set Beam.rideId rideId,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.ScheduledPayout Domain.Types.ScheduledPayout.ScheduledPayout where
  fromTType' (Beam.ScheduledPayoutT {..}) = do
    pure $
      Just
        Domain.Types.ScheduledPayout.ScheduledPayout
          { amount = amount,
            bookingId = bookingId,
            createdAt = createdAt,
            driverId = driverId,
            expectedCreditTime = expectedCreditTime,
            failureReason = failureReason,
            id = Kernel.Types.Id.Id id,
            payoutTransactionId = payoutTransactionId,
            retryCount = retryCount,
            rideId = rideId,
            status = status,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.ScheduledPayout Domain.Types.ScheduledPayout.ScheduledPayout where
  toTType' (Domain.Types.ScheduledPayout.ScheduledPayout {..}) = do
    Beam.ScheduledPayoutT
      { Beam.amount = amount,
        Beam.bookingId = bookingId,
        Beam.createdAt = createdAt,
        Beam.driverId = driverId,
        Beam.expectedCreditTime = expectedCreditTime,
        Beam.failureReason = failureReason,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.payoutTransactionId = payoutTransactionId,
        Beam.retryCount = retryCount,
        Beam.rideId = rideId,
        Beam.status = status,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
