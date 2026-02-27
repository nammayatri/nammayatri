{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.ReconciliationSummary where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.ReconciliationSummary
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.ReconciliationSummary as Beam
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary] -> m ())
createMany = traverse_ create

findByDateAndType ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.UTCTime -> Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationType -> m ([Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary]))
findByDateAndType reconciliationDate reconciliationType = do findAllWithKV [Se.And [Se.Is Beam.reconciliationDate $ Se.Eq reconciliationDate, Se.Is Beam.reconciliationType $ Se.Eq reconciliationType]]

findByDateRange ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.UTCTime -> Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationType -> m ([Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary]))
findByDateRange reconciliationDate reconciliationType = do findAllWithKV [Se.And [Se.Is Beam.reconciliationDate $ Se.Eq reconciliationDate, Se.Is Beam.reconciliationType $ Se.Eq reconciliationType]]

findById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary -> m (Maybe Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByMerchantId :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary]))
findByMerchantId merchantId = do findAllWithKV [Se.Is Beam.merchantId $ Se.Eq merchantId]

updateStatus ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Finance.Domain.Types.ReconciliationSummary.JobStatus -> Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary -> m ())
updateStatus status id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary -> m (Maybe Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.errorMessage errorMessage,
      Se.Set Beam.matchRate matchRate,
      Se.Set Beam.matchedRecords matchedRecords,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.reconciliationDate reconciliationDate,
      Se.Set Beam.reconciliationType reconciliationType,
      Se.Set Beam.sourceTotal sourceTotal,
      Se.Set Beam.status status,
      Se.Set Beam.targetTotal targetTotal,
      Se.Set Beam.totalDiscrepancies totalDiscrepancies,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.varianceAmount varianceAmount
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.ReconciliationSummary Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary where
  fromTType' (Beam.ReconciliationSummaryT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary
          { createdAt = createdAt,
            errorMessage = errorMessage,
            id = Kernel.Types.Id.Id id,
            matchRate = matchRate,
            matchedRecords = matchedRecords,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            reconciliationDate = reconciliationDate,
            reconciliationType = reconciliationType,
            sourceTotal = sourceTotal,
            status = status,
            targetTotal = targetTotal,
            totalDiscrepancies = totalDiscrepancies,
            updatedAt = updatedAt,
            varianceAmount = varianceAmount
          }

instance ToTType' Beam.ReconciliationSummary Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary where
  toTType' (Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary {..}) = do
    Beam.ReconciliationSummaryT
      { Beam.createdAt = createdAt,
        Beam.errorMessage = errorMessage,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.matchRate = matchRate,
        Beam.matchedRecords = matchedRecords,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.reconciliationDate = reconciliationDate,
        Beam.reconciliationType = reconciliationType,
        Beam.sourceTotal = sourceTotal,
        Beam.status = status,
        Beam.targetTotal = targetTotal,
        Beam.totalDiscrepancies = totalDiscrepancies,
        Beam.updatedAt = updatedAt,
        Beam.varianceAmount = varianceAmount
      }
