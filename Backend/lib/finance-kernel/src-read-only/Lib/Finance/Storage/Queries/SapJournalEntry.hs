{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.SapJournalEntry (module Lib.Finance.Storage.Queries.SapJournalEntry, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.SapJournalEntry
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.SapJournalEntry as Beam
import Lib.Finance.Storage.Queries.SapJournalEntryExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.SapJournalEntry.SapJournalEntry -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.SapJournalEntry.SapJournalEntry] -> m ())
createMany = traverse_ create

findByBatchId :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.SapJournalEntry.SapJournalEntry]))
findByBatchId batchId = do findAllWithKV [Se.Is Beam.batchId $ Se.Eq batchId]

findByBelnr :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.SapJournalEntry.SapJournalEntry]))
findByBelnr belnr = do findAllWithKV [Se.Is Beam.belnr $ Se.Eq belnr]

findById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.SapJournalEntry.SapJournalEntry -> m (Maybe Lib.Finance.Domain.Types.SapJournalEntry.SapJournalEntry))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.SapJournalEntry.SapJournalEntry -> m (Maybe Lib.Finance.Domain.Types.SapJournalEntry.SapJournalEntry))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.SapJournalEntry.SapJournalEntry -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.SapJournalEntry.SapJournalEntry {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.batchId batchId,
      Se.Set Beam.belnr belnr,
      Se.Set Beam.blart blart,
      Se.Set Beam.bldat bldat,
      Se.Set Beam.budat budat,
      Se.Set Beam.currency currency,
      Se.Set Beam.description description,
      Se.Set Beam.gjahr gjahr,
      Se.Set Beam.glName glName,
      Se.Set Beam.glNumber glNumber,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.rawResponse rawResponse,
      Se.Set Beam.sapMessage sapMessage,
      Se.Set Beam.status status,
      Se.Set Beam.totalCreditAmount totalCreditAmount,
      Se.Set Beam.totalDebitAmount totalDebitAmount,
      Se.Set Beam.transactionCount transactionCount,
      Se.Set Beam.transactionType transactionType,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
