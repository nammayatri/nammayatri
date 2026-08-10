{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Queries.OrphanInstances.ReconSettlementOrder where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Finance.Domain.Types.ReconSettlementOrder
import qualified Lib.Finance.Storage.Beam.ReconSettlementOrder as Beam

instance FromTType' Beam.ReconSettlementOrder Lib.Finance.Domain.Types.ReconSettlementOrder.ReconSettlementOrder where
  fromTType' (Beam.ReconSettlementOrderT {..}) = do
    pure $
      Just
        Lib.Finance.Domain.Types.ReconSettlementOrder.ReconSettlementOrder
          { allocatedBankCash = allocatedBankCash,
            bffAmount = bffAmount,
            bffType = bffType,
            claimedGrossAmount = claimedGrossAmount,
            claimedSettlementAmount = claimedSettlementAmount,
            correctionForOrderRowId = Kernel.Types.Id.Id <$> correctionForOrderRowId,
            createdAt = createdAt,
            deductionByCollector = deductionByCollector,
            diffAmount = diffAmount,
            driverId = driverId,
            id = Kernel.Types.Id.Id id,
            invoiceNo = invoiceNo,
            manualConfirmationReason = manualConfirmationReason,
            manuallyConfirmedAt = manuallyConfirmedAt,
            manuallyConfirmedBy = manuallyConfirmedBy,
            merchantId = merchantId,
            merchantOperatingCityId = merchantOperatingCityId,
            messageId = messageId,
            orderId = orderId,
            orderSequence = orderSequence,
            orderState = orderState,
            orderTransactionId = orderTransactionId,
            ourReconStatus = ourReconStatus,
            paymentStatus = paymentStatus,
            payoutEligible = payoutEligible,
            platformGrossFare = platformGrossFare,
            platformNetReceivable = platformNetReceivable,
            rawJson = rawJson,
            reasonCode = reasonCode,
            receivedAt = receivedAt,
            reconTransactionId = reconTransactionId,
            reconciliationStatus = reconciliationStatus,
            refundReference = refundReference,
            refundStatus = refundStatus,
            refundedAt = refundedAt,
            remarks = remarks,
            rideId = rideId,
            settlementClearedAt = settlementClearedAt,
            settlementDate = settlementDate,
            settlementDetailIndex = settlementDetailIndex,
            settlementId = settlementId,
            settlementReferenceNo = settlementReferenceNo,
            settlementType = settlementType,
            sourceType = sourceType,
            updatedAt = updatedAt,
            utrSettlementId = Kernel.Types.Id.Id <$> utrSettlementId,
            wireOrderReconStatus = wireOrderReconStatus,
            wireReconStatus = wireReconStatus,
            withholdingTaxGst = withholdingTaxGst,
            withholdingTaxTds = withholdingTaxTds
          }

instance ToTType' Beam.ReconSettlementOrder Lib.Finance.Domain.Types.ReconSettlementOrder.ReconSettlementOrder where
  toTType' (Lib.Finance.Domain.Types.ReconSettlementOrder.ReconSettlementOrder {..}) = do
    Beam.ReconSettlementOrderT
      { Beam.allocatedBankCash = allocatedBankCash,
        Beam.bffAmount = bffAmount,
        Beam.bffType = bffType,
        Beam.claimedGrossAmount = claimedGrossAmount,
        Beam.claimedSettlementAmount = claimedSettlementAmount,
        Beam.correctionForOrderRowId = Kernel.Types.Id.getId <$> correctionForOrderRowId,
        Beam.createdAt = createdAt,
        Beam.deductionByCollector = deductionByCollector,
        Beam.diffAmount = diffAmount,
        Beam.driverId = driverId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.invoiceNo = invoiceNo,
        Beam.manualConfirmationReason = manualConfirmationReason,
        Beam.manuallyConfirmedAt = manuallyConfirmedAt,
        Beam.manuallyConfirmedBy = manuallyConfirmedBy,
        Beam.merchantId = merchantId,
        Beam.merchantOperatingCityId = merchantOperatingCityId,
        Beam.messageId = messageId,
        Beam.orderId = orderId,
        Beam.orderSequence = orderSequence,
        Beam.orderState = orderState,
        Beam.orderTransactionId = orderTransactionId,
        Beam.ourReconStatus = ourReconStatus,
        Beam.paymentStatus = paymentStatus,
        Beam.payoutEligible = payoutEligible,
        Beam.platformGrossFare = platformGrossFare,
        Beam.platformNetReceivable = platformNetReceivable,
        Beam.rawJson = rawJson,
        Beam.reasonCode = reasonCode,
        Beam.receivedAt = receivedAt,
        Beam.reconTransactionId = reconTransactionId,
        Beam.reconciliationStatus = reconciliationStatus,
        Beam.refundReference = refundReference,
        Beam.refundStatus = refundStatus,
        Beam.refundedAt = refundedAt,
        Beam.remarks = remarks,
        Beam.rideId = rideId,
        Beam.settlementClearedAt = settlementClearedAt,
        Beam.settlementDate = settlementDate,
        Beam.settlementDetailIndex = settlementDetailIndex,
        Beam.settlementId = settlementId,
        Beam.settlementReferenceNo = settlementReferenceNo,
        Beam.settlementType = settlementType,
        Beam.sourceType = sourceType,
        Beam.updatedAt = updatedAt,
        Beam.utrSettlementId = Kernel.Types.Id.getId <$> utrSettlementId,
        Beam.wireOrderReconStatus = wireOrderReconStatus,
        Beam.wireReconStatus = wireReconStatus,
        Beam.withholdingTaxGst = withholdingTaxGst,
        Beam.withholdingTaxTds = withholdingTaxTds
      }
