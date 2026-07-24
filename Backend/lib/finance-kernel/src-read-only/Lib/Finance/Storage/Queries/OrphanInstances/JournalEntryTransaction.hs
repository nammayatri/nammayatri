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
            creditAmount = creditAmount,
            currency = currency,
            debitAmount = debitAmount,
            description = description,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            sapBatchId = sapBatchId,
            sapJournalEntryId = sapJournalEntryId,
            status = status,
            subscriptionId = subscriptionId,
            transactionType = transactionType,
            updatedAt = updatedAt
          }

instance ToTType' Beam.JournalEntryTransaction Lib.Finance.Domain.Types.JournalEntryTransaction.JournalEntryTransaction where
  toTType' (Lib.Finance.Domain.Types.JournalEntryTransaction.JournalEntryTransaction {..}) = do
    Beam.JournalEntryTransactionT
      { Beam.createdAt = createdAt,
        Beam.createdBy = createdBy,
        Beam.creditAmount = creditAmount,
        Beam.currency = currency,
        Beam.debitAmount = debitAmount,
        Beam.description = description,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.sapBatchId = sapBatchId,
        Beam.sapJournalEntryId = sapJournalEntryId,
        Beam.status = status,
        Beam.subscriptionId = subscriptionId,
        Beam.transactionType = transactionType,
        Beam.updatedAt = updatedAt
      }
