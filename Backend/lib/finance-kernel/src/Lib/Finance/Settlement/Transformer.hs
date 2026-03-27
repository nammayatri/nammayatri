{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.Settlement.Transformer
  ( toPgPaymentSettlementReport,
    mapTxnType,
  )
where

import qualified Kernel.External.Settlement.Interface.Types as Ext
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (generateGUID, getCurrentTime)
import qualified Lib.Finance.Domain.Types.PgPaymentSettlementReport as Dom
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow

toPgPaymentSettlementReport ::
  (BeamFlow.BeamFlow m r) =>
  Text ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  Ext.PaymentSettlementReport ->
  m Dom.PgPaymentSettlementReport
toPgPaymentSettlementReport merchantId merchantOperatingCityId referenceId referenceType report = do
  now <- getCurrentTime
  reportId <- generateGUID
  let (chargebackId, chargebackReasonCode, chargebackStatus, chargebackAmount) = case report.txnType of
        Ext.CHARGEBACK ->
          ( report.disputeId,
            Nothing,
            Just "INITIATED",
            Just report.txnAmount
          )
        _ -> (Nothing, Nothing, Nothing, Nothing)
  pure
    Dom.PgPaymentSettlementReport
      { id = Id reportId,
        merchantId = merchantId,
        merchantOperatingCityId = merchantOperatingCityId,
        orderId = report.orderId,
        txnId = report.txnId,
        rrn = report.rrn,
        utr = report.utr,
        bankId = Nothing,
        chargebackAmount = chargebackAmount,
        txnType = mapTxnType report.txnType,
        txnStatus = mapTxnStatus report.txnStatus,
        txnDate = report.txnDate,
        txnAmount = report.txnAmount,
        pgBaseFee = report.pgBaseFee,
        pgTax = report.pgTax,
        settlementAmount = report.settlementAmount,
        currency = report.currency,
        vendorId = report.vendorId,
        uniqueSplitId = report.uniqueSplitId,
        paymentGateway = report.paymentGateway,
        pgApprovalCode = Nothing,
        paymentMethod = mapPaymentMethod <$> report.paymentMethod,
        paymentMethodSubType = report.paymentMethodSubType,
        settlementType = mapSettlementType <$> report.settlementType,
        settlementMode = mapSettlementMode <$> report.settlementMode,
        settlementId = report.settlementId,
        settlementDate = report.settlementDate,
        referenceId = referenceId,
        referenceType = referenceType,
        refundId = report.refundId,
        refundArn = report.refundArn,
        refundDate = report.refundDate,
        refundAmount = report.refundAmount,
        refundBaseFee = report.refundBaseFee,
        refundTax = report.refundTax,
        refundReasonCode = Nothing,
        refundMethod = Nothing,
        chargebackId = chargebackId,
        chargebackReasonCode = chargebackReasonCode,
        chargebackStatus = chargebackStatus,
        disputeId = report.disputeId,
        disputeType = mapDisputeType <$> report.disputeType,
        reconStatus = Dom.PENDING,
        reconMessage = Nothing,
        rawData = report.rawData,
        createdAt = now,
        updatedAt = now
      }

mapTxnType :: Ext.TxnType -> Dom.TxnType
mapTxnType Ext.ORDER = Dom.ORDER
mapTxnType Ext.REFUND = Dom.REFUND
mapTxnType Ext.CHARGEBACK = Dom.CHARGEBACK

mapTxnStatus :: Ext.TxnStatus -> Dom.TxnStatus
mapTxnStatus Ext.SUCCESS = Dom.SUCCESS
mapTxnStatus Ext.FAILED = Dom.FAILED

mapPaymentMethod :: Ext.PaymentMethodType -> Dom.PaymentMethod
mapPaymentMethod Ext.UPI = Dom.UPI
mapPaymentMethod Ext.CREDIT_CARD = Dom.CREDIT_CARD
mapPaymentMethod Ext.DEBIT_CARD = Dom.DEBIT_CARD
mapPaymentMethod Ext.NETBANKING = Dom.NETBANKING
mapPaymentMethod Ext.WALLET = Dom.WALLET

mapSettlementType :: Ext.SettlementType -> Dom.SettlementType
mapSettlementType Ext.CREDIT = Dom.CREDIT
mapSettlementType Ext.DEBIT = Dom.DEBIT

mapSettlementMode :: Ext.SettlementMode -> Dom.SettlementMode
mapSettlementMode Ext.GROSS = Dom.GROSS
mapSettlementMode Ext.NET = Dom.NET
mapSettlementMode Ext.NETTING = Dom.NETTING

mapDisputeType :: Ext.DisputeType -> Dom.DisputeType
mapDisputeType Ext.FRAUD = Dom.FRAUD
mapDisputeType Ext.CONSUMER = Dom.CONSUMER
mapDisputeType Ext.PROCESSING_ERROR = Dom.PROCESSING_ERROR
mapDisputeType Ext.OTHER_DISPUTE = Dom.OTHER
