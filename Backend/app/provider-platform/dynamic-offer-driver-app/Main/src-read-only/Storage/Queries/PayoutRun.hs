{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PayoutRun where

import qualified Data.Time
import qualified Domain.Types.PayoutRun
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PayoutRun as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PayoutRun.PayoutRun -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PayoutRun.PayoutRun] -> m ())
createMany = traverse_ create

findAllByIds :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Kernel.Types.Id.Id Domain.Types.PayoutRun.PayoutRun] -> m [Domain.Types.PayoutRun.PayoutRun])
findAllByIds id = do findAllWithKV [Se.And [Se.Is Beam.id $ Se.In (Kernel.Types.Id.getId <$> id)]]

findByParentJobId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m (Maybe Domain.Types.PayoutRun.PayoutRun))
findByParentJobId parentJobId = do findOneWithKV [Se.Is Beam.parentJobId $ Se.Eq parentJobId]

findByParentJobIdAndValueDate :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> Data.Time.Day -> m (Maybe Domain.Types.PayoutRun.PayoutRun))
findByParentJobIdAndValueDate parentJobId valueDate = do findOneWithKV [Se.And [Se.Is Beam.parentJobId $ Se.Eq parentJobId, Se.Is Beam.valueDate $ Se.Eq valueDate]]

sealRun ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.PayoutRun.PayoutRunStatus -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.PayoutRun.PayoutRun -> m ())
sealRun status totalAmount batchCount sealedAt id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.totalAmount totalAmount,
      Se.Set Beam.batchCount batchCount,
      Se.Set Beam.sealedAt sealedAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateAssemblyCounts :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Int -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.PayoutRun.PayoutRun -> m ())
updateAssemblyCounts evaluatedCount excludedCount includedCount id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.evaluatedCount evaluatedCount,
      Se.Set Beam.excludedCount excludedCount,
      Se.Set Beam.includedCount includedCount,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateResolutionCounts ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.PayoutRun.PayoutRunStatus -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.PayoutRun.PayoutRun -> m ())
updateResolutionCounts status paidCount failedCount pendingCount paidAmount failedAmount debitedAmount resolvedAt id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.paidCount paidCount,
      Se.Set Beam.failedCount failedCount,
      Se.Set Beam.pendingCount pendingCount,
      Se.Set Beam.paidAmount paidAmount,
      Se.Set Beam.failedAmount failedAmount,
      Se.Set Beam.debitedAmount debitedAmount,
      Se.Set Beam.resolvedAt resolvedAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PayoutRun.PayoutRunStatus -> Kernel.Types.Id.Id Domain.Types.PayoutRun.PayoutRun -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PayoutRun.PayoutRun -> m (Maybe Domain.Types.PayoutRun.PayoutRun))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PayoutRun.PayoutRun -> m ())
updateByPrimaryKey (Domain.Types.PayoutRun.PayoutRun {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.batchCount batchCount,
      Se.Set Beam.currency currency,
      Se.Set Beam.debitedAmount debitedAmount,
      Se.Set Beam.evaluatedCount evaluatedCount,
      Se.Set Beam.excludedCount excludedCount,
      Se.Set Beam.failedAmount failedAmount,
      Se.Set Beam.failedCount failedCount,
      Se.Set Beam.includedCount includedCount,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.origin origin,
      Se.Set Beam.paidAmount paidAmount,
      Se.Set Beam.paidCount paidCount,
      Se.Set Beam.parentJobId parentJobId,
      Se.Set Beam.payoutPartner payoutPartner,
      Se.Set Beam.pendingCount pendingCount,
      Se.Set Beam.resolvedAt resolvedAt,
      Se.Set Beam.scheduledFor scheduledFor,
      Se.Set Beam.sealedAt sealedAt,
      Se.Set Beam.status status,
      Se.Set Beam.totalAmount totalAmount,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.valueDate valueDate
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PayoutRun Domain.Types.PayoutRun.PayoutRun where
  fromTType' (Beam.PayoutRunT {..}) = do
    pure $
      Just
        Domain.Types.PayoutRun.PayoutRun
          { batchCount = batchCount,
            createdAt = createdAt,
            currency = currency,
            debitedAmount = debitedAmount,
            evaluatedCount = evaluatedCount,
            excludedCount = excludedCount,
            failedAmount = failedAmount,
            failedCount = failedCount,
            id = Kernel.Types.Id.Id id,
            includedCount = includedCount,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            origin = origin,
            paidAmount = paidAmount,
            paidCount = paidCount,
            parentJobId = parentJobId,
            payoutPartner = payoutPartner,
            pendingCount = pendingCount,
            resolvedAt = resolvedAt,
            scheduledFor = scheduledFor,
            sealedAt = sealedAt,
            status = status,
            totalAmount = totalAmount,
            updatedAt = updatedAt,
            valueDate = valueDate
          }

instance ToTType' Beam.PayoutRun Domain.Types.PayoutRun.PayoutRun where
  toTType' (Domain.Types.PayoutRun.PayoutRun {..}) = do
    Beam.PayoutRunT
      { Beam.batchCount = batchCount,
        Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.debitedAmount = debitedAmount,
        Beam.evaluatedCount = evaluatedCount,
        Beam.excludedCount = excludedCount,
        Beam.failedAmount = failedAmount,
        Beam.failedCount = failedCount,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.includedCount = includedCount,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.origin = origin,
        Beam.paidAmount = paidAmount,
        Beam.paidCount = paidCount,
        Beam.parentJobId = parentJobId,
        Beam.payoutPartner = payoutPartner,
        Beam.pendingCount = pendingCount,
        Beam.resolvedAt = resolvedAt,
        Beam.scheduledFor = scheduledFor,
        Beam.sealedAt = sealedAt,
        Beam.status = status,
        Beam.totalAmount = totalAmount,
        Beam.updatedAt = updatedAt,
        Beam.valueDate = valueDate
      }
