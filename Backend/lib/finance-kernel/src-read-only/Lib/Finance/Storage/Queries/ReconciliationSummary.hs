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
import qualified Lib.Finance.Reconciliation.Types
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.ReconciliationSummary as Beam
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary] -> m ())
createMany = traverse_ create

findByChunk ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Finance.Reconciliation.Types.Domain -> Lib.Finance.Reconciliation.Types.DataSource -> Lib.Finance.Reconciliation.Types.DataSource -> Kernel.Prelude.UTCTime -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary]))
findByChunk domain source target reconciliationDate merchantId merchantOperatingCityId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.source $ Se.Eq source,
          Se.Is Beam.target $ Se.Eq target,
          Se.Is Beam.reconciliationDate $ Se.Eq reconciliationDate,
          Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId
        ]
    ]

findByDomainSourceTarget ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Finance.Reconciliation.Types.Domain -> Lib.Finance.Reconciliation.Types.DataSource -> Lib.Finance.Reconciliation.Types.DataSource -> m ([Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary]))
findByDomainSourceTarget domain source target = do findAllWithKV [Se.And [Se.Is Beam.domain $ Se.Eq domain, Se.Is Beam.source $ Se.Eq source, Se.Is Beam.target $ Se.Eq target]]

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
    [ Se.Set Beam.disputeAmountTotal disputeAmountTotal,
      Se.Set Beam.domain domain,
      Se.Set Beam.errorMessage errorMessage,
      Se.Set Beam.matchRate matchRate,
      Se.Set Beam.matchedRecords matchedRecords,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.reconciliationDate reconciliationDate,
      Se.Set Beam.source source,
      Se.Set Beam.sourceTotal sourceTotal,
      Se.Set Beam.status status,
      Se.Set Beam.target target,
      Se.Set Beam.targetTotal targetTotal,
      Se.Set Beam.totalDiscrepancies totalDiscrepancies,
      Se.Set Beam.totalRecords totalRecords,
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
            disputeAmountTotal = disputeAmountTotal,
            domain = domain,
            errorMessage = errorMessage,
            id = Kernel.Types.Id.Id id,
            matchRate = matchRate,
            matchedRecords = matchedRecords,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            reconciliationDate = reconciliationDate,
            source = source,
            sourceTotal = sourceTotal,
            status = status,
            target = target,
            targetTotal = targetTotal,
            totalDiscrepancies = totalDiscrepancies,
            totalRecords = totalRecords,
            updatedAt = updatedAt,
            varianceAmount = varianceAmount
          }

instance ToTType' Beam.ReconciliationSummary Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary where
  toTType' (Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary {..}) = do
    Beam.ReconciliationSummaryT
      { Beam.createdAt = createdAt,
        Beam.disputeAmountTotal = disputeAmountTotal,
        Beam.domain = domain,
        Beam.errorMessage = errorMessage,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.matchRate = matchRate,
        Beam.matchedRecords = matchedRecords,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.reconciliationDate = reconciliationDate,
        Beam.source = source,
        Beam.sourceTotal = sourceTotal,
        Beam.status = status,
        Beam.target = target,
        Beam.targetTotal = targetTotal,
        Beam.totalDiscrepancies = totalDiscrepancies,
        Beam.totalRecords = totalRecords,
        Beam.updatedAt = updatedAt,
        Beam.varianceAmount = varianceAmount
      }
