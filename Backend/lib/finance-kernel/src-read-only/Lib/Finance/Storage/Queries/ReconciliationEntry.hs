{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.ReconciliationEntry where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.ReconciliationEntry
import qualified Lib.Finance.Domain.Types.ReconciliationSummary
import qualified Lib.Finance.Reconciliation.Types
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.ReconciliationEntry as Beam
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry] -> m ())
createMany = traverse_ create

findByDomainSourceTarget ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Finance.Reconciliation.Types.Domain -> Lib.Finance.Reconciliation.Types.DataSource -> Lib.Finance.Reconciliation.Types.DataSource -> m ([Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry]))
findByDomainSourceTarget domain source target = do findAllWithKV [Se.And [Se.Is Beam.domain $ Se.Eq domain, Se.Is Beam.source $ Se.Eq source, Se.Is Beam.target $ Se.Eq target]]

findByEntityId :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry]))
findByEntityId entityId = do findAllWithKV [Se.Is Beam.entityId $ Se.Eq entityId]

findByGroupTargetKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry]))
findByGroupTargetKey groupTargetKey = do findAllWithKV [Se.Is Beam.groupTargetKey $ Se.Eq groupTargetKey]

findById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry -> m (Maybe Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByReconciliationStatus ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Finance.Reconciliation.Types.ReconciliationStatus -> m ([Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry]))
findByReconciliationStatus reconStatus = do findAllWithKV [Se.Is Beam.reconStatus $ Se.Eq reconStatus]

findBySummaryId ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary -> m ([Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry]))
findBySummaryId summaryId = do findAllWithKV [Se.Is Beam.summaryId $ Se.Eq (Kernel.Types.Id.getId summaryId)]

findByPrimaryKey ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry -> m (Maybe Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.actualAmount actualAmount,
      Se.Set Beam.closeReason closeReason,
      Se.Set Beam.component component,
      Se.Set Beam.domain domain,
      Se.Set Beam.entityId entityId,
      Se.Set Beam.entityMeta entityMeta,
      Se.Set Beam.entryKey entryKey,
      Se.Set Beam.expectedAmount expectedAmount,
      Se.Set Beam.firstSeenAt firstSeenAt,
      Se.Set Beam.groupSourceTotal groupSourceTotal,
      Se.Set Beam.groupTargetAmount groupTargetAmount,
      Se.Set Beam.groupTargetKey groupTargetKey,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.mismatchReason mismatchReason,
      Se.Set Beam.open open,
      Se.Set Beam.partyId partyId,
      Se.Set Beam.reconStatus reconStatus,
      Se.Set Beam.reconciliationDate reconciliationDate,
      Se.Set Beam.resolvedAt resolvedAt,
      Se.Set Beam.rrn rrn,
      Se.Set Beam.settlementDate settlementDate,
      Se.Set Beam.settlementId settlementId,
      Se.Set Beam.settlementMode settlementMode,
      Se.Set Beam.source source,
      Se.Set Beam.sourceLifecycle sourceLifecycle,
      Se.Set Beam.sourceRecordId sourceRecordId,
      Se.Set Beam.summaryId (Kernel.Types.Id.getId summaryId),
      Se.Set Beam.target target,
      Se.Set Beam.targetRecordId targetRecordId,
      Se.Set Beam.timestamp timestamp,
      Se.Set Beam.transactionDate transactionDate,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.variance variance
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.ReconciliationEntry Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry where
  fromTType' (Beam.ReconciliationEntryT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry
          { actualAmount = actualAmount,
            closeReason = closeReason,
            component = component,
            createdAt = createdAt,
            domain = domain,
            entityId = entityId,
            entityMeta = entityMeta,
            entryKey = entryKey,
            expectedAmount = expectedAmount,
            firstSeenAt = firstSeenAt,
            groupSourceTotal = groupSourceTotal,
            groupTargetAmount = groupTargetAmount,
            groupTargetKey = groupTargetKey,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            mismatchReason = mismatchReason,
            open = open,
            partyId = partyId,
            reconStatus = reconStatus,
            reconciliationDate = reconciliationDate,
            resolvedAt = resolvedAt,
            rrn = rrn,
            settlementDate = settlementDate,
            settlementId = settlementId,
            settlementMode = settlementMode,
            source = source,
            sourceLifecycle = sourceLifecycle,
            sourceRecordId = sourceRecordId,
            summaryId = Kernel.Types.Id.Id summaryId,
            target = target,
            targetRecordId = targetRecordId,
            timestamp = timestamp,
            transactionDate = transactionDate,
            updatedAt = updatedAt,
            variance = variance
          }

instance ToTType' Beam.ReconciliationEntry Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry where
  toTType' (Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry {..}) = do
    Beam.ReconciliationEntryT
      { Beam.actualAmount = actualAmount,
        Beam.closeReason = closeReason,
        Beam.component = component,
        Beam.createdAt = createdAt,
        Beam.domain = domain,
        Beam.entityId = entityId,
        Beam.entityMeta = entityMeta,
        Beam.entryKey = entryKey,
        Beam.expectedAmount = expectedAmount,
        Beam.firstSeenAt = firstSeenAt,
        Beam.groupSourceTotal = groupSourceTotal,
        Beam.groupTargetAmount = groupTargetAmount,
        Beam.groupTargetKey = groupTargetKey,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.mismatchReason = mismatchReason,
        Beam.open = open,
        Beam.partyId = partyId,
        Beam.reconStatus = reconStatus,
        Beam.reconciliationDate = reconciliationDate,
        Beam.resolvedAt = resolvedAt,
        Beam.rrn = rrn,
        Beam.settlementDate = settlementDate,
        Beam.settlementId = settlementId,
        Beam.settlementMode = settlementMode,
        Beam.source = source,
        Beam.sourceLifecycle = sourceLifecycle,
        Beam.sourceRecordId = sourceRecordId,
        Beam.summaryId = Kernel.Types.Id.getId summaryId,
        Beam.target = target,
        Beam.targetRecordId = targetRecordId,
        Beam.timestamp = timestamp,
        Beam.transactionDate = transactionDate,
        Beam.updatedAt = updatedAt,
        Beam.variance = variance
      }
