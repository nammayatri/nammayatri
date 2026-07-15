{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.OrphanInstances.SapJournalEntry where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.SapJournalEntry
import qualified Lib.Finance.Storage.Beam.SapJournalEntry as Beam

instance FromTType' Beam.SapJournalEntry Lib.Finance.Domain.Types.SapJournalEntry.SapJournalEntry where
  fromTType' (Beam.SapJournalEntryT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.SapJournalEntry.SapJournalEntry
          { batchId = batchId,
            belnr = belnr,
            blart = blart,
            bldat = bldat,
            budat = budat,
            createdAt = createdAt,
            createdBy = createdBy,
            createdById = createdById,
            currency = currency,
            description = description,
            gjahr = gjahr,
            glName = glName,
            glNumber = glNumber,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            periodEndTime = periodEndTime,
            periodStartTime = periodStartTime,
            rawResponse = rawResponse,
            sapMessage = sapMessage,
            status = status,
            totalCreditAmount = totalCreditAmount,
            totalDebitAmount = totalDebitAmount,
            transactionCount = transactionCount,
            transactionType = transactionType,
            updatedAt = updatedAt,
            updatedBy = updatedBy,
            updatedById = updatedById
          }

instance ToTType' Beam.SapJournalEntry Lib.Finance.Domain.Types.SapJournalEntry.SapJournalEntry where
  toTType' (Lib.Finance.Domain.Types.SapJournalEntry.SapJournalEntry {..}) = do
    Beam.SapJournalEntryT
      { Beam.batchId = batchId,
        Beam.belnr = belnr,
        Beam.blart = blart,
        Beam.bldat = bldat,
        Beam.budat = budat,
        Beam.createdAt = createdAt,
        Beam.createdBy = createdBy,
        Beam.createdById = createdById,
        Beam.currency = currency,
        Beam.description = description,
        Beam.gjahr = gjahr,
        Beam.glName = glName,
        Beam.glNumber = glNumber,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.periodEndTime = periodEndTime,
        Beam.periodStartTime = periodStartTime,
        Beam.rawResponse = rawResponse,
        Beam.sapMessage = sapMessage,
        Beam.status = status,
        Beam.totalCreditAmount = totalCreditAmount,
        Beam.totalDebitAmount = totalDebitAmount,
        Beam.transactionCount = transactionCount,
        Beam.transactionType = transactionType,
        Beam.updatedAt = updatedAt,
        Beam.updatedBy = updatedBy,
        Beam.updatedById = updatedById
      }
