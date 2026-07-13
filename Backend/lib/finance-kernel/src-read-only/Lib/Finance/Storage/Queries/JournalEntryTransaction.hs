{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.JournalEntryTransaction (module Lib.Finance.Storage.Queries.JournalEntryTransaction, module ReExport) where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.JournalEntryTransaction
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.JournalEntryTransaction as Beam
import Lib.Finance.Storage.Queries.JournalEntryTransactionExtra as ReExport
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.JournalEntryTransaction.JournalEntryTransaction -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.JournalEntryTransaction.JournalEntryTransaction] -> m ())
createMany = traverse_ create

findById ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.JournalEntryTransaction.JournalEntryTransaction -> m (Maybe Lib.Finance.Domain.Types.JournalEntryTransaction.JournalEntryTransaction))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.JournalEntryTransaction.JournalEntryTransaction -> m (Maybe Lib.Finance.Domain.Types.JournalEntryTransaction.JournalEntryTransaction))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.JournalEntryTransaction.JournalEntryTransaction -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.JournalEntryTransaction.JournalEntryTransaction {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdBy createdBy,
      Se.Set Beam.creditAmount creditAmount,
      Se.Set Beam.currency currency,
      Se.Set Beam.debitAmount debitAmount,
      Se.Set Beam.description description,
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.sapBatchId sapBatchId,
      Se.Set Beam.sapJournalEntryId sapJournalEntryId,
      Se.Set Beam.status status,
      Se.Set Beam.subscriptionId subscriptionId,
      Se.Set Beam.transactionType transactionType,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
