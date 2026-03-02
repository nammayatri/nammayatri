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
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.ReconciliationEntry as Beam
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry] -> m ())
createMany = traverse_ create

findByBookingId :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry]))
findByBookingId bookingId = do findAllWithKV [Se.Is Beam.bookingId $ Se.Eq bookingId]

findByDateAndType ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.UTCTime -> Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationType -> m ([Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry]))
findByDateAndType reconciliationDate reconciliationType = do findAllWithKV [Se.And [Se.Is Beam.reconciliationDate $ Se.Eq reconciliationDate, Se.Is Beam.reconciliationType $ Se.Eq reconciliationType]]

findByDcoId :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry]))
findByDcoId dcoId = do findAllWithKV [Se.Is Beam.dcoId $ Se.Eq dcoId]

findById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry -> m (Maybe Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByReconciliationStatus ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationStatus -> m ([Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry]))
findByReconciliationStatus reconStatus = do findAllWithKV [Se.Is Beam.reconStatus $ Se.Eq reconStatus]

findBySummaryId ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationSummary -> m ([Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry]))
findBySummaryId summaryId = do findAllWithKV [Se.Is Beam.summaryId $ Se.Eq (Kernel.Types.Id.getId summaryId)]

findExceptions ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.UTCTime -> Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationType -> m ([Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry]))
findExceptions reconciliationDate reconciliationType = do findAllWithKV [Se.And [Se.Is Beam.reconciliationDate $ Se.Eq reconciliationDate, Se.Is Beam.reconciliationType $ Se.Eq reconciliationType]]

findByPrimaryKey ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry -> m (Maybe Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.actualLedgerValue actualLedgerValue,
      Se.Set Beam.bookingId bookingId,
      Se.Set Beam.dcoId dcoId,
      Se.Set Beam.expectedDsrValue expectedDsrValue,
      Se.Set Beam.financeComponent financeComponent,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.mismatchReason mismatchReason,
      Se.Set Beam.mode mode,
      Se.Set Beam.reconStatus reconStatus,
      Se.Set Beam.reconciliationDate reconciliationDate,
      Se.Set Beam.reconciliationType reconciliationType,
      Se.Set Beam.sourceDetails sourceDetails,
      Se.Set Beam.status status,
      Se.Set Beam.summaryId (Kernel.Types.Id.getId summaryId),
      Se.Set Beam.targetDetails targetDetails,
      Se.Set Beam.timestamp timestamp,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.variance variance
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.ReconciliationEntry Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry where
  fromTType' (Beam.ReconciliationEntryT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry
          { actualLedgerValue = actualLedgerValue,
            bookingId = bookingId,
            createdAt = createdAt,
            dcoId = dcoId,
            expectedDsrValue = expectedDsrValue,
            financeComponent = financeComponent,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            mismatchReason = mismatchReason,
            mode = mode,
            reconStatus = reconStatus,
            reconciliationDate = reconciliationDate,
            reconciliationType = reconciliationType,
            sourceDetails = sourceDetails,
            status = status,
            summaryId = Kernel.Types.Id.Id summaryId,
            targetDetails = targetDetails,
            timestamp = timestamp,
            updatedAt = updatedAt,
            variance = variance
          }

instance ToTType' Beam.ReconciliationEntry Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry where
  toTType' (Lib.Finance.Domain.Types.ReconciliationEntry.ReconciliationEntry {..}) = do
    Beam.ReconciliationEntryT
      { Beam.actualLedgerValue = actualLedgerValue,
        Beam.bookingId = bookingId,
        Beam.createdAt = createdAt,
        Beam.dcoId = dcoId,
        Beam.expectedDsrValue = expectedDsrValue,
        Beam.financeComponent = financeComponent,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.mismatchReason = mismatchReason,
        Beam.mode = mode,
        Beam.reconStatus = reconStatus,
        Beam.reconciliationDate = reconciliationDate,
        Beam.reconciliationType = reconciliationType,
        Beam.sourceDetails = sourceDetails,
        Beam.status = status,
        Beam.summaryId = Kernel.Types.Id.getId summaryId,
        Beam.targetDetails = targetDetails,
        Beam.timestamp = timestamp,
        Beam.updatedAt = updatedAt,
        Beam.variance = variance
      }
