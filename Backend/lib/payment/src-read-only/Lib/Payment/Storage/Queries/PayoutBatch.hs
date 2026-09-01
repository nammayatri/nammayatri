{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.PayoutBatch (module Lib.Payment.Storage.Queries.PayoutBatch, module ReExport) where

import qualified Data.Time
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PayoutBatch
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.PayoutBatch as Beam
import Lib.Payment.Storage.Queries.PayoutBatchExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch -> m ())
create = createWithKV

createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch] -> m ())
createMany = traverse_ create

findAllByRunId :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m [Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch])
findAllByRunId runId = do findAllWithKV [Se.Is Beam.runId $ Se.Eq runId]

findByClientRefNo :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m (Maybe Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch))
findByClientRefNo clientRefNo = do findOneWithKV [Se.Is Beam.clientRefNo $ Se.Eq clientRefNo]

markSubmitted ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch -> m ())
markSubmitted status partnerBatchRef submittedAt id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.partnerBatchRef partnerBatchRef,
      Se.Set Beam.submittedAt submittedAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateFailure ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch -> m ())
updateFailure status partnerResponseCode failureReason id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.partnerResponseCode partnerResponseCode,
      Se.Set Beam.failureReason failureReason,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateInquiryState ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Int -> Kernel.Prelude.Maybe Data.Time.Day -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch -> m ())
updateInquiryState inquiryAttemptsToday inquiryQuotaDate nextInquiryAt id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.inquiryAttemptsToday inquiryAttemptsToday,
      Se.Set Beam.inquiryQuotaDate inquiryQuotaDate,
      Se.Set Beam.nextInquiryAt nextInquiryAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateResolutionCounts ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchStatus -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch -> m ())
updateResolutionCounts status processedCount rejectedCount pendingCount resolvedAt id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.processedCount processedCount,
      Se.Set Beam.rejectedCount rejectedCount,
      Se.Set Beam.pendingCount pendingCount,
      Se.Set Beam.resolvedAt resolvedAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchStatus -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch -> m ())
updateStatus status id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch -> m (Maybe Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.clientRefNo clientRefNo,
      Se.Set Beam.failureReason failureReason,
      Se.Set Beam.inquiryAttemptsToday inquiryAttemptsToday,
      Se.Set Beam.inquiryQuotaDate inquiryQuotaDate,
      Se.Set Beam.itemCount itemCount,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.nextInquiryAt nextInquiryAt,
      Se.Set Beam.origin origin,
      Se.Set Beam.partnerBatchRef partnerBatchRef,
      Se.Set Beam.partnerResponseCode partnerResponseCode,
      Se.Set Beam.payoutRail payoutRail,
      Se.Set Beam.pendingCount pendingCount,
      Se.Set Beam.processedCount processedCount,
      Se.Set Beam.rejectedCount rejectedCount,
      Se.Set Beam.resolvedAt resolvedAt,
      Se.Set Beam.retryOfBatchId (Kernel.Types.Id.getId <$> retryOfBatchId),
      Se.Set Beam.runId runId,
      Se.Set Beam.status status,
      Se.Set Beam.submittedAt submittedAt,
      Se.Set Beam.totalAmount totalAmount,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.valueDate valueDate
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
