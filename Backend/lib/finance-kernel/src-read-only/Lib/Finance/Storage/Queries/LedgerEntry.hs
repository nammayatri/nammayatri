{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.LedgerEntry where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.Account
import qualified Lib.Finance.Domain.Types.LedgerEntry
import qualified Lib.Finance.Storage.Beam.BeamFlow
import qualified Lib.Finance.Storage.Beam.LedgerEntry as Beam
import qualified Sequelize as Se

create :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry -> m ())
create = createWithKV

createMany :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry] -> m ())
createMany = traverse_ create

findByFromAccount :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account -> m ([Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry]))
findByFromAccount fromAccountId = do findAllWithKV [Se.Is Beam.fromAccountId $ Se.Eq (Kernel.Types.Id.getId fromAccountId)]

findById :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry -> m (Maybe Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByOwner :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry]))
findByOwner ownerType ownerId = do findAllWithKV [Se.And [Se.Is Beam.ownerType $ Se.Eq ownerType, Se.Is Beam.ownerId $ Se.Eq ownerId]]

findByOwnerAndStatus ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Text -> Lib.Finance.Domain.Types.LedgerEntry.EntryStatus -> m ([Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry]))
findByOwnerAndStatus ownerType ownerId status = do findAllWithKV [Se.And [Se.Is Beam.ownerType $ Se.Eq ownerType, Se.Is Beam.ownerId $ Se.Eq ownerId, Se.Is Beam.status $ Se.Eq status]]

findByReference :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ([Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry]))
findByReference referenceType referenceId = do findAllWithKV [Se.And [Se.Is Beam.referenceType $ Se.Eq referenceType, Se.Is Beam.referenceId $ Se.Eq referenceId]]

findByStatus :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.LedgerEntry.EntryStatus -> m ([Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry]))
findByStatus status = do findAllWithKV [Se.Is Beam.status $ Se.Eq status]

findByToAccount :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account -> m ([Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry]))
findByToAccount toAccountId = do findAllWithKV [Se.Is Beam.toAccountId $ Se.Eq (Kernel.Types.Id.getId toAccountId)]

findReversalOf ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry) -> m (Maybe Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry))
findReversalOf reversalOf = do findOneWithKV [Se.Is Beam.reversalOf $ Se.Eq (Kernel.Types.Id.getId <$> reversalOf)]

updateSettled ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Finance.Domain.Types.LedgerEntry.EntryStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry -> m ())
updateSettled status settledAt id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.status status, Se.Set Beam.settledAt settledAt, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateStatus :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.LedgerEntry.EntryStatus -> Kernel.Types.Id.Id Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry -> m ())
updateStatus status id = do _now <- getCurrentTime; updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateVoided ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Finance.Domain.Types.LedgerEntry.EntryStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Types.Id.Id Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry -> m ())
updateVoided status voidReason id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.status status, Se.Set Beam.voidReason voidReason, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry -> m (Maybe Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (Lib.Finance.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry -> m ())
updateByPrimaryKey (Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.currency currency,
      Se.Set Beam.entryNumber entryNumber,
      Se.Set Beam.entryType entryType,
      Se.Set Beam.fromAccountId (Kernel.Types.Id.getId fromAccountId),
      Se.Set Beam.merchantId merchantId,
      Se.Set Beam.merchantOperatingCityId merchantOperatingCityId,
      Se.Set Beam.metadata metadata,
      Se.Set Beam.ownerId ownerId,
      Se.Set Beam.ownerType ownerType,
      Se.Set Beam.referenceId referenceId,
      Se.Set Beam.referenceType referenceType,
      Se.Set Beam.reversalOf (Kernel.Types.Id.getId <$> reversalOf),
      Se.Set Beam.settledAt settledAt,
      Se.Set Beam.status status,
      Se.Set Beam.timestamp timestamp,
      Se.Set Beam.toAccountId (Kernel.Types.Id.getId toAccountId),
      Se.Set Beam.voidReason voidReason,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.LedgerEntry Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry where
  fromTType' (Beam.LedgerEntryT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry
          { amount = amount,
            createdAt = createdAt,
            currency = currency,
            entryNumber = entryNumber,
            entryType = entryType,
            fromAccountId = Kernel.Types.Id.Id fromAccountId,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            metadata = metadata,
            ownerId = ownerId,
            ownerType = ownerType,
            referenceId = referenceId,
            referenceType = referenceType,
            reversalOf = Kernel.Types.Id.Id <$> reversalOf,
            settledAt = settledAt,
            status = status,
            timestamp = timestamp,
            toAccountId = Kernel.Types.Id.Id toAccountId,
            voidReason = voidReason,
            updatedAt = updatedAt
          }

instance ToTType' Beam.LedgerEntry Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry where
  toTType' (Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry {..}) = do
    Beam.LedgerEntryT
      { Beam.amount = amount,
        Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.entryNumber = entryNumber,
        Beam.entryType = entryType,
        Beam.fromAccountId = Kernel.Types.Id.getId fromAccountId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.metadata = metadata,
        Beam.ownerId = ownerId,
        Beam.ownerType = ownerType,
        Beam.referenceId = referenceId,
        Beam.referenceType = referenceType,
        Beam.reversalOf = Kernel.Types.Id.getId <$> reversalOf,
        Beam.settledAt = settledAt,
        Beam.status = status,
        Beam.timestamp = timestamp,
        Beam.toAccountId = Kernel.Types.Id.getId toAccountId,
        Beam.voidReason = voidReason,
        Beam.updatedAt = updatedAt
      }
