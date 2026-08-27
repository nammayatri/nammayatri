{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.OrphanInstances.JournalEntryTransaction where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.JournalEntryTransaction
import qualified Lib.Finance.Storage.Beam.JournalEntryTransaction as Beam

instance FromTType' Beam.JournalEntryTransaction Lib.Finance.Domain.Types.JournalEntryTransaction.JournalEntryTransaction where
  fromTType' (Beam.JournalEntryTransactionT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.JournalEntryTransaction.JournalEntryTransaction
          { createdAt = createdAt,
            createdBy = createdBy,
            createdById = createdById,
            creditAmount = creditAmount,
            currency = currency,
            debitAmount = debitAmount,
            description = description,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            referenceId = referenceId,
            referenceType = referenceType,
            sapBatchId = sapBatchId,
            sapJournalEntryId = Kernel.Types.Id.Id sapJournalEntryId,
            status = status,
            transactionType = transactionType,
            updatedAt = updatedAt,
            updatedBy = updatedBy,
            updatedById = updatedById
          }

instance ToTType' Beam.JournalEntryTransaction Lib.Finance.Domain.Types.JournalEntryTransaction.JournalEntryTransaction where
  toTType' (Lib.Finance.Domain.Types.JournalEntryTransaction.JournalEntryTransaction {..}) = do
    Beam.JournalEntryTransactionT
      { Beam.createdAt = createdAt,
        Beam.createdBy = createdBy,
        Beam.createdById = createdById,
        Beam.creditAmount = creditAmount,
        Beam.currency = currency,
        Beam.debitAmount = debitAmount,
        Beam.description = description,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.referenceId = referenceId,
        Beam.referenceType = referenceType,
        Beam.sapBatchId = sapBatchId,
        Beam.sapJournalEntryId = Kernel.Types.Id.getId sapJournalEntryId,
        Beam.status = status,
        Beam.transactionType = transactionType,
        Beam.updatedAt = updatedAt,
        Beam.updatedBy = updatedBy,
        Beam.updatedById = updatedById
      }
