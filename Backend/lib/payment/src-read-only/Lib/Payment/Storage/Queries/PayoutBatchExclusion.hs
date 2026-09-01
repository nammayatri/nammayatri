{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Payment.Storage.Queries.PayoutBatchExclusion where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Payment.Domain.Types.PayoutBatchExclusion
import qualified Lib.Payment.Storage.Beam.BeamFlow
import qualified Lib.Payment.Storage.Beam.PayoutBatchExclusion as Beam
import qualified Sequelize as Se

create :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PayoutBatchExclusion.PayoutBatchExclusion -> m ())
create = createWithKV

createMany :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Payment.Domain.Types.PayoutBatchExclusion.PayoutBatchExclusion] -> m ())
createMany = traverse_ create

findAllByBeneficiaryId :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m ([Lib.Payment.Domain.Types.PayoutBatchExclusion.PayoutBatchExclusion]))
findAllByBeneficiaryId beneficiaryId = do findAllWithKV [Se.Is Beam.beneficiaryId $ Se.Eq beneficiaryId]

findAllByRunId :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m ([Lib.Payment.Domain.Types.PayoutBatchExclusion.PayoutBatchExclusion]))
findAllByRunId runId = do findAllWithKV [Se.Is Beam.runId $ Se.Eq runId]

markCorrected ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutBatchExclusion.PayoutBatchExclusion -> m ())
markCorrected correctedAt id = do _now <- getCurrentTime; updateOneWithKV [Se.Set Beam.correctedAt correctedAt, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Payment.Domain.Types.PayoutBatchExclusion.PayoutBatchExclusion -> m (Maybe Lib.Payment.Domain.Types.PayoutBatchExclusion.PayoutBatchExclusion))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Payment.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Payment.Domain.Types.PayoutBatchExclusion.PayoutBatchExclusion -> m ())
updateByPrimaryKey (Lib.Payment.Domain.Types.PayoutBatchExclusion.PayoutBatchExclusion {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.balanceAtEvaluation balanceAtEvaluation,
      Se.Set Beam.beneficiaryId beneficiaryId,
      Se.Set Beam.beneficiaryType beneficiaryType,
      Se.Set Beam.correctedAt correctedAt,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.notifiedAt notifiedAt,
      Se.Set Beam.reason reason,
      Se.Set Beam.runId runId,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PayoutBatchExclusion Lib.Payment.Domain.Types.PayoutBatchExclusion.PayoutBatchExclusion where
  fromTType' (Beam.PayoutBatchExclusionT {..}) = do
    pure $
      Just
        Lib.Payment.Domain.Types.PayoutBatchExclusion.PayoutBatchExclusion
          { balanceAtEvaluation = balanceAtEvaluation,
            beneficiaryId = beneficiaryId,
            beneficiaryType = beneficiaryType,
            correctedAt = correctedAt,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            notifiedAt = notifiedAt,
            reason = reason,
            runId = runId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PayoutBatchExclusion Lib.Payment.Domain.Types.PayoutBatchExclusion.PayoutBatchExclusion where
  toTType' (Lib.Payment.Domain.Types.PayoutBatchExclusion.PayoutBatchExclusion {..}) = do
    Beam.PayoutBatchExclusionT
      { Beam.balanceAtEvaluation = balanceAtEvaluation,
        Beam.beneficiaryId = beneficiaryId,
        Beam.beneficiaryType = beneficiaryType,
        Beam.correctedAt = correctedAt,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.notifiedAt = notifiedAt,
        Beam.reason = reason,
        Beam.runId = runId,
        Beam.updatedAt = updatedAt
      }
