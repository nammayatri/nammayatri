{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.PayoutBatch (module Lib.Payment.Storage.Queries.PayoutBatch, module ReExport) where

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

updateAfterStatusCall ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Int -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch -> m ())
updateAfterStatusCall status resolvedAt failureReason failureCode statusCheckRound statusCheckCalls nextStatusCallAt statusNoDataReplies id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.resolvedAt resolvedAt,
      Se.Set Beam.failureReason failureReason,
      Se.Set Beam.failureCode failureCode,
      Se.Set Beam.statusCheckRound statusCheckRound,
      Se.Set Beam.statusCheckCalls statusCheckCalls,
      Se.Set Beam.nextStatusCallAt nextStatusCallAt,
      Se.Set Beam.statusNoDataReplies statusNoDataReplies,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateAfterSubmit ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Int -> Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch -> m ())
updateAfterSubmit status partnerBatchRef submittedAt statusCheckRound statusCheckCalls nextStatusCallAt failureReason failureCode id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.partnerBatchRef partnerBatchRef,
      Se.Set Beam.submittedAt submittedAt,
      Se.Set Beam.statusCheckRound statusCheckRound,
      Se.Set Beam.statusCheckCalls statusCheckCalls,
      Se.Set Beam.nextStatusCallAt nextStatusCallAt,
      Se.Set Beam.failureReason failureReason,
      Se.Set Beam.failureCode failureCode,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateFailure ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Payment.Domain.Types.PayoutBatch.PayoutBatchStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch -> m ())
updateFailure status failureReason failureCode resolvedAt nextStatusCallAt id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.status status,
      Se.Set Beam.failureReason failureReason,
      Se.Set Beam.failureCode failureCode,
      Se.Set Beam.resolvedAt resolvedAt,
      Se.Set Beam.nextStatusCallAt nextStatusCallAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch -> m (Maybe Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.PayoutBatch.PayoutBatch {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.clientRefNo clientRefNo,
      Se.Set Beam.excludedCount excludedCount,
      Se.Set Beam.executionDate executionDate,
      Se.Set Beam.failureCode failureCode,
      Se.Set Beam.failureReason failureReason,
      Se.Set Beam.itemCount itemCount,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.nextStatusCallAt nextStatusCallAt,
      Se.Set Beam.origin origin,
      Se.Set Beam.partnerBatchRef partnerBatchRef,
      Se.Set Beam.payoutRail payoutRail,
      Se.Set Beam.payoutServiceName payoutServiceName,
      Se.Set Beam.resolvedAt resolvedAt,
      Se.Set Beam.status status,
      Se.Set Beam.statusCheckCalls statusCheckCalls,
      Se.Set Beam.statusCheckRound statusCheckRound,
      Se.Set Beam.statusNoDataReplies statusNoDataReplies,
      Se.Set Beam.submittedAt submittedAt,
      Se.Set Beam.totalAmount totalAmount,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
