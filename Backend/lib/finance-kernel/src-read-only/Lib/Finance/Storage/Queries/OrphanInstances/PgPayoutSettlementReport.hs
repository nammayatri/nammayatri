{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.OrphanInstances.PgPayoutSettlementReport where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.PgPayoutSettlementReport
import qualified Lib.Finance.Storage.Beam.PgPayoutSettlementReport as Beam

instance FromTType' Beam.PgPayoutSettlementReport Lib.Finance.Domain.Types.PgPayoutSettlementReport.PgPayoutSettlementReport where
  fromTType' (Beam.PgPayoutSettlementReportT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.PgPayoutSettlementReport.PgPayoutSettlementReport
          { bankName = bankName,
            beneficiaryAccountNumber = beneficiaryAccountNumber,
            beneficiaryIfsc = beneficiaryIfsc,
            beneficiaryType = beneficiaryType,
            createdAt = createdAt,
            currency = currency,
            fulfillmentAmount = fulfillmentAmount,
            fulfillmentDate = fulfillmentDate,
            fulfillmentId = fulfillmentId,
            fulfillmentInstrumentType = fulfillmentInstrumentType,
            fulfillmentMethod = fulfillmentMethod,
            fulfillmentOrderCreatedAt = fulfillmentOrderCreatedAt,
            fulfillmentResponseCode = fulfillmentResponseCode,
            fulfillmentResponseMessage = fulfillmentResponseMessage,
            fulfillmentStatus = fulfillmentStatus,
            fulfillmentTxnId = fulfillmentTxnId,
            id = Kernel.Types.Id.Id id,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            orderId = orderId,
            paymentGateway = paymentGateway,
            payoutCustomerId = payoutCustomerId,
            payoutRequestId = payoutRequestId,
            rawData = rawData,
            reconMessage = reconMessage,
            reconStatus = reconStatus,
            referenceType = referenceType,
            rrn = rrn,
            settlementAmount = settlementAmount,
            settlementDate = settlementDate,
            settlementId = settlementId,
            settlementMode = settlementMode,
            settlementType = settlementType,
            txnAmount = txnAmount,
            txnDate = txnDate,
            txnId = txnId,
            txnStatus = txnStatus,
            updatedAt = updatedAt,
            utr = utr
          }

instance ToTType' Beam.PgPayoutSettlementReport Lib.Finance.Domain.Types.PgPayoutSettlementReport.PgPayoutSettlementReport where
  toTType' (Lib.Finance.Domain.Types.PgPayoutSettlementReport.PgPayoutSettlementReport {..}) = do
    Beam.PgPayoutSettlementReportT
      { Beam.bankName = bankName,
        Beam.beneficiaryAccountNumber = beneficiaryAccountNumber,
        Beam.beneficiaryIfsc = beneficiaryIfsc,
        Beam.beneficiaryType = beneficiaryType,
        Beam.createdAt = createdAt,
        Beam.currency = currency,
        Beam.fulfillmentAmount = fulfillmentAmount,
        Beam.fulfillmentDate = fulfillmentDate,
        Beam.fulfillmentId = fulfillmentId,
        Beam.fulfillmentInstrumentType = fulfillmentInstrumentType,
        Beam.fulfillmentMethod = fulfillmentMethod,
        Beam.fulfillmentOrderCreatedAt = fulfillmentOrderCreatedAt,
        Beam.fulfillmentResponseCode = fulfillmentResponseCode,
        Beam.fulfillmentResponseMessage = fulfillmentResponseMessage,
        Beam.fulfillmentStatus = fulfillmentStatus,
        Beam.fulfillmentTxnId = fulfillmentTxnId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.orderId = orderId,
        Beam.paymentGateway = paymentGateway,
        Beam.payoutCustomerId = payoutCustomerId,
        Beam.payoutRequestId = payoutRequestId,
        Beam.rawData = rawData,
        Beam.reconMessage = reconMessage,
        Beam.reconStatus = reconStatus,
        Beam.referenceType = referenceType,
        Beam.rrn = rrn,
        Beam.settlementAmount = settlementAmount,
        Beam.settlementDate = settlementDate,
        Beam.settlementId = settlementId,
        Beam.settlementMode = settlementMode,
        Beam.settlementType = settlementType,
        Beam.txnAmount = txnAmount,
        Beam.txnDate = txnDate,
        Beam.txnId = txnId,
        Beam.txnStatus = txnStatus,
        Beam.updatedAt = updatedAt,
        Beam.utr = utr
      }
