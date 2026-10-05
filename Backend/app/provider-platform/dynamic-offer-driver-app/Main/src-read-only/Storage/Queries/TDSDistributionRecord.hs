{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.TDSDistributionRecord where

import qualified Domain.Types.Person
import qualified Domain.Types.TDSDistributionBatch
import qualified Domain.Types.TDSDistributionRecord
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.TDSDistributionRecord as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TDSDistributionRecord.TDSDistributionRecord -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.TDSDistributionRecord.TDSDistributionRecord] -> m ())
createMany = traverse_ create

findAllByBatchId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.TDSDistributionBatch.TDSDistributionBatch) -> m ([Domain.Types.TDSDistributionRecord.TDSDistributionRecord]))
findAllByBatchId batchId = do findAllWithKV [Se.Is Beam.batchId $ Se.Eq (Kernel.Types.Id.getId <$> batchId)]

findAllByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> m ([Domain.Types.TDSDistributionRecord.TDSDistributionRecord]))
findAllByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId <$> driverId)]

findAllByStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TDSDistributionRecord.TDSDistributionStatus -> m ([Domain.Types.TDSDistributionRecord.TDSDistributionRecord]))
findAllByStatus status = do findAllWithKV [Se.Is Beam.status $ Se.Eq status]

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.TDSDistributionRecord.TDSDistributionRecord -> m (Maybe Domain.Types.TDSDistributionRecord.TDSDistributionRecord))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateDelivered ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.TDSDistributionRecord.TDSDistributionStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Domain.Types.TDSDistributionRecord.TDSFailureReason -> Kernel.Types.Id.Id Domain.Types.TDSDistributionRecord.TDSDistributionRecord -> m ())
updateDelivered status deliveredAt failureReason id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.deliveredAt deliveredAt,
      Se.Set Beam.failureReason failureReason,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateForResend ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.TDSDistributionBatch.TDSDistributionBatch) -> Domain.Types.TDSDistributionRecord.TDSDistributionStatus -> Kernel.Prelude.Int -> Kernel.Prelude.Maybe Domain.Types.TDSDistributionRecord.TDSFailureReason -> Kernel.Types.Id.Id Domain.Types.TDSDistributionRecord.TDSDistributionRecord -> m ())
updateForResend batchId status retryCount failureReason id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.batchId (Kernel.Types.Id.getId <$> batchId),
      Se.Set Beam.status status,
      Se.Set Beam.retryCount retryCount,
      Se.Set Beam.failureReason failureReason,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.TDSDistributionRecord.TDSDistributionStatus -> Kernel.Types.Id.Id Domain.Types.TDSDistributionRecord.TDSDistributionRecord -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatusAndFailureReason ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.TDSDistributionRecord.TDSDistributionStatus -> Kernel.Prelude.Maybe Domain.Types.TDSDistributionRecord.TDSFailureReason -> Kernel.Types.Id.Id Domain.Types.TDSDistributionRecord.TDSDistributionRecord -> m ())
updateStatusAndFailureReason status failureReason id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.failureReason failureReason, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatusAndRetryCount ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.TDSDistributionRecord.TDSDistributionStatus -> Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.TDSDistributionRecord.TDSDistributionRecord -> m ())
updateStatusAndRetryCount status retryCount id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.retryCount retryCount, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.TDSDistributionRecord.TDSDistributionRecord -> m (Maybe Domain.Types.TDSDistributionRecord.TDSDistributionRecord))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.TDSDistributionRecord.TDSDistributionRecord -> m ())
updateByPrimaryKey (Domain.Types.TDSDistributionRecord.TDSDistributionRecord {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.assessmentYear assessmentYear,
      Se.Set Beam.attemptCount attemptCount,
      Se.Set Beam.batchId (Kernel.Types.Id.getId <$> batchId),
      Se.Set Beam.deliveredAt deliveredAt,
      Se.Set Beam.driverId (Kernel.Types.Id.getId <$> driverId),
      Se.Set Beam.emailAddress emailAddress,
      Se.Set Beam.failureReason failureReason,
      Se.Set Beam.fileName fileName,
      Se.Set Beam.financialYear financialYear,
      Se.Set Beam.lastAttemptAt lastAttemptAt,
      Se.Set Beam.latestEmailDeliveryId (Kernel.Types.Id.getId <$> latestEmailDeliveryId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.quarter quarter,
      Se.Set Beam.retryCount retryCount,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.TDSDistributionRecord Domain.Types.TDSDistributionRecord.TDSDistributionRecord where
  fromTType' (Beam.TDSDistributionRecordT {..}) = do
    pure $
      Just
        Domain.Types.TDSDistributionRecord.TDSDistributionRecord
          { assessmentYear = assessmentYear,
            attemptCount = attemptCount,
            batchId = Kernel.Types.Id.Id <$> batchId,
            createdAt = createdAt,
            deliveredAt = deliveredAt,
            driverId = Kernel.Types.Id.Id <$> driverId,
            emailAddress = emailAddress,
            failureReason = failureReason,
            fileName = fileName,
            financialYear = financialYear,
            id = Kernel.Types.Id.Id id,
            lastAttemptAt = lastAttemptAt,
            latestEmailDeliveryId = Kernel.Types.Id.Id <$> latestEmailDeliveryId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            quarter = quarter,
            retryCount = retryCount,
            status = status,
            updatedAt = updatedAt
          }

instance ToTType' Beam.TDSDistributionRecord Domain.Types.TDSDistributionRecord.TDSDistributionRecord where
  toTType' (Domain.Types.TDSDistributionRecord.TDSDistributionRecord {..}) = do
    Beam.TDSDistributionRecordT
      { Beam.assessmentYear = assessmentYear,
        Beam.attemptCount = attemptCount,
        Beam.batchId = Kernel.Types.Id.getId <$> batchId,
        Beam.createdAt = createdAt,
        Beam.deliveredAt = deliveredAt,
        Beam.driverId = Kernel.Types.Id.getId <$> driverId,
        Beam.emailAddress = emailAddress,
        Beam.failureReason = failureReason,
        Beam.fileName = fileName,
        Beam.financialYear = financialYear,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lastAttemptAt = lastAttemptAt,
        Beam.latestEmailDeliveryId = Kernel.Types.Id.getId <$> latestEmailDeliveryId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.quarter = quarter,
        Beam.retryCount = retryCount,
        Beam.status = status,
        Beam.updatedAt = updatedAt
      }
