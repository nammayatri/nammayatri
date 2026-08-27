module Lib.Finance.Storage.Queries.JournalEntryTransactionExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Lib.Finance.Domain.Types.JournalEntryTransaction as Domain (JournalEntryTransaction)
import Lib.Finance.Domain.Types.SapJournalEntry
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.JournalEntryTransaction as Beam
import Lib.Finance.Storage.Queries.OrphanInstances.JournalEntryTransaction ()
import qualified Sequelize as Se

findByMerchantIdWithFilters ::
  (BeamFlow m r) =>
  Text ->
  Text ->
  TransactionType ->
  Maybe Text ->
  Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int ->
  m [Domain.JournalEntryTransaction]
findByMerchantIdWithFilters merchantId merchantOperatingCityId transactionType mbReferenceId batchId mbDescription mbLimit mbOffset =
  findAllWithOptionsKV
    [ Se.And $
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq merchantOperatingCityId,
          Se.Is Beam.transactionType $ Se.Eq transactionType,
          Se.Is Beam.sapBatchId $ Se.Eq batchId
        ]
          <> [Se.Is Beam.referenceId $ Se.Eq (Just refId) | Just refId <- [mbReferenceId]]
          <> [Se.Is Beam.description $ Se.Eq description | Just description <- [mbDescription]]
    ]
    (Se.Desc Beam.createdAt)
    (Just $ min 100 $ fromMaybe 20 mbLimit)
    mbOffset
