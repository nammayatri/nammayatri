{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.OrphanInstances.RsfReconLedgerEntry where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.RsfReconLedgerEntry
import qualified Lib.Finance.Storage.Beam.RsfReconLedgerEntry as Beam

instance FromTType' Beam.RsfReconLedgerEntry Lib.Finance.Domain.Types.RsfReconLedgerEntry.RsfReconLedgerEntry where
  fromTType' (Beam.RsfReconLedgerEntryT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.RsfReconLedgerEntry.RsfReconLedgerEntry
          { actorId = actorId,
            actorType = actorType,
            amount = amount,
            bapId = bapId,
            bapUri = bapUri,
            bffAmount = bffAmount,
            bffType = bffType,
            claimStatus = claimStatus,
            collectorAppId = collectorAppId,
            contextTransactionId = contextTransactionId,
            counterpartyReconStatus = counterpartyReconStatus,
            createdAt = createdAt,
            currency = currency,
            deductionByCollector = deductionByCollector,
            diffMessageCode = diffMessageCode,
            diffMessageName = diffMessageName,
            driverId = driverId,
            effectiveAt = effectiveAt,
            entryType = entryType,
            id = Kernel.Types.Id.Id id,
            invoiceNo = invoiceNo,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            messageId = messageId,
            orderId = orderId,
            orderPaymentAmount = orderPaymentAmount,
            orderState = orderState,
            orderTransactionId = orderTransactionId,
            rawJson = rawJson,
            reason = reason,
            rejectionDiff = rejectionDiff,
            reportedAt = reportedAt,
            reportedInMessageId = reportedInMessageId,
            rideId = rideId,
            settlementId = settlementId,
            settlementReasonCode = settlementReasonCode,
            source = source,
            ttl = ttl,
            updatedAt = updatedAt,
            utr = utr,
            withholdingTaxGst = withholdingTaxGst,
            withholdingTaxTds = withholdingTaxTds
          }

instance ToTType' Beam.RsfReconLedgerEntry Lib.Finance.Domain.Types.RsfReconLedgerEntry.RsfReconLedgerEntry where
  toTType' (Lib.Finance.Domain.Types.RsfReconLedgerEntry.RsfReconLedgerEntry {..}) = do
    Beam.RsfReconLedgerEntryT
      { Beam.actorId = actorId,
        Beam.actorType = actorType,
        Beam.amount = amount,
        Beam.bapId = bapId,
        Beam.bapUri = bapUri,
        Beam.bffAmount = bffAmount,
        Beam.bffType = bffType,
        Beam.claimStatus = claimStatus,
        Beam.collectorAppId = collectorAppId,
        Beam.contextTransactionId = contextTransactionId,
        Beam.counterpartyReconStatus = counterpartyReconStatus,
        Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.deductionByCollector = deductionByCollector,
        Beam.diffMessageCode = diffMessageCode,
        Beam.diffMessageName = diffMessageName,
        Beam.driverId = driverId,
        Beam.effectiveAt = effectiveAt,
        Beam.entryType = entryType,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.invoiceNo = invoiceNo,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.messageId = messageId,
        Beam.orderId = orderId,
        Beam.orderPaymentAmount = orderPaymentAmount,
        Beam.orderState = orderState,
        Beam.orderTransactionId = orderTransactionId,
        Beam.rawJson = rawJson,
        Beam.reason = reason,
        Beam.rejectionDiff = rejectionDiff,
        Beam.reportedAt = reportedAt,
        Beam.reportedInMessageId = reportedInMessageId,
        Beam.rideId = rideId,
        Beam.settlementId = settlementId,
        Beam.settlementReasonCode = settlementReasonCode,
        Beam.source = source,
        Beam.ttl = ttl,
        Beam.updatedAt = updatedAt,
        Beam.utr = utr,
        Beam.withholdingTaxGst = withholdingTaxGst,
        Beam.withholdingTaxTds = withholdingTaxTds
      }
