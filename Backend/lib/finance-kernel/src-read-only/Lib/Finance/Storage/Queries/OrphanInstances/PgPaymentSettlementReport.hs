{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Lib.Finance.Storage.Queries.OrphanInstances.PgPaymentSettlementReport where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Lib.Finance.Domain.Types.PgPaymentSettlementReport
import qualified Lib.Finance.Storage.Beam.PgPaymentSettlementReport as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.PgPaymentSettlementReport Lib.Finance.Domain.Types.PgPaymentSettlementReport.PgPaymentSettlementReport
    where fromTType' (Beam.PgPaymentSettlementReportT {..}) = do pure $ Just Lib.Finance.Domain.Types.PgPaymentSettlementReport.PgPaymentSettlementReport{bankId = bankId,
                                                                                                                                                          chargebackAmount = chargebackAmount,
                                                                                                                                                          chargebackId = chargebackId,
                                                                                                                                                          chargebackReasonCode = chargebackReasonCode,
                                                                                                                                                          chargebackStatus = chargebackStatus,
                                                                                                                                                          createdAt = createdAt,
                                                                                                                                                          currency = currency,
                                                                                                                                                          disputeId = disputeId,
                                                                                                                                                          disputeType = disputeType,
                                                                                                                                                          id = Kernel.Types.Id.Id id,
                                                                                                                                                          merchantId = merchantId,
                                                                                                                                                          merchantOperatingCityId = merchantOperatingCityId,
                                                                                                                                                          orderId = orderId,
                                                                                                                                                          paymentGateway = paymentGateway,
                                                                                                                                                          paymentMethod = paymentMethod,
                                                                                                                                                          paymentMethodSubType = paymentMethodSubType,
                                                                                                                                                          pgApprovalCode = pgApprovalCode,
                                                                                                                                                          pgBaseFee = pgBaseFee,
                                                                                                                                                          pgTax = pgTax,
                                                                                                                                                          rawData = rawData,
                                                                                                                                                          reconMessage = reconMessage,
                                                                                                                                                          reconStatus = reconStatus,
                                                                                                                                                          referenceId = referenceId,
                                                                                                                                                          referenceType = referenceType,
                                                                                                                                                          refundAmount = refundAmount,
                                                                                                                                                          refundArn = refundArn,
                                                                                                                                                          refundBaseFee = refundBaseFee,
                                                                                                                                                          refundDate = refundDate,
                                                                                                                                                          refundId = refundId,
                                                                                                                                                          refundMethod = refundMethod,
                                                                                                                                                          refundReasonCode = refundReasonCode,
                                                                                                                                                          refundTax = refundTax,
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
                                                                                                                                                          txnType = txnType,
                                                                                                                                                          uniqueSplitId = uniqueSplitId,
                                                                                                                                                          updatedAt = updatedAt,
                                                                                                                                                          utr = utr,
                                                                                                                                                          vendorId = vendorId}
instance ToTType' Beam.PgPaymentSettlementReport Lib.Finance.Domain.Types.PgPaymentSettlementReport.PgPaymentSettlementReport
    where toTType' (Lib.Finance.Domain.Types.PgPaymentSettlementReport.PgPaymentSettlementReport {..}) = do Beam.PgPaymentSettlementReportT{Beam.bankId = bankId,
                                                                                                                                            Beam.chargebackAmount = chargebackAmount,
                                                                                                                                            Beam.chargebackId = chargebackId,
                                                                                                                                            Beam.chargebackReasonCode = chargebackReasonCode,
                                                                                                                                            Beam.chargebackStatus = chargebackStatus,
                                                                                                                                            Beam.createdAt = createdAt,
                                                                                                                                            Beam.currency = currency,
                                                                                                                                            Beam.disputeId = disputeId,
                                                                                                                                            Beam.disputeType = disputeType,
                                                                                                                                            Beam.id = Kernel.Types.Id.getId id,
                                                                                                                                            Beam.merchantId = merchantId,
                                                                                                                                            Beam.merchantOperatingCityId = merchantOperatingCityId,
                                                                                                                                            Beam.orderId = orderId,
                                                                                                                                            Beam.paymentGateway = paymentGateway,
                                                                                                                                            Beam.paymentMethod = paymentMethod,
                                                                                                                                            Beam.paymentMethodSubType = paymentMethodSubType,
                                                                                                                                            Beam.pgApprovalCode = pgApprovalCode,
                                                                                                                                            Beam.pgBaseFee = pgBaseFee,
                                                                                                                                            Beam.pgTax = pgTax,
                                                                                                                                            Beam.rawData = rawData,
                                                                                                                                            Beam.reconMessage = reconMessage,
                                                                                                                                            Beam.reconStatus = reconStatus,
                                                                                                                                            Beam.referenceId = referenceId,
                                                                                                                                            Beam.referenceType = referenceType,
                                                                                                                                            Beam.refundAmount = refundAmount,
                                                                                                                                            Beam.refundArn = refundArn,
                                                                                                                                            Beam.refundBaseFee = refundBaseFee,
                                                                                                                                            Beam.refundDate = refundDate,
                                                                                                                                            Beam.refundId = refundId,
                                                                                                                                            Beam.refundMethod = refundMethod,
                                                                                                                                            Beam.refundReasonCode = refundReasonCode,
                                                                                                                                            Beam.refundTax = refundTax,
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
                                                                                                                                            Beam.txnType = txnType,
                                                                                                                                            Beam.uniqueSplitId = uniqueSplitId,
                                                                                                                                            Beam.updatedAt = updatedAt,
                                                                                                                                            Beam.utr = utr,
                                                                                                                                            Beam.vendorId = vendorId}



