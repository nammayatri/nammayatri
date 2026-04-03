{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Lib.Finance.Storage.Beam.PgPaymentSettlementReport where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.PgPaymentSettlementReport
import qualified Data.Aeson
import qualified Database.Beam as B



data PgPaymentSettlementReportT f
    = PgPaymentSettlementReportT {bankId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                  chargebackAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                                  chargebackId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                  chargebackReasonCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                  chargebackStatus :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                  createdAt :: (B.C f Kernel.Prelude.UTCTime),
                                  currency :: (B.C f Kernel.Types.Common.Currency),
                                  disputeId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                  disputeType :: (B.C f (Kernel.Prelude.Maybe Lib.Finance.Domain.Types.PgPaymentSettlementReport.DisputeType)),
                                  id :: (B.C f Kernel.Prelude.Text),
                                  merchantId :: (B.C f Kernel.Prelude.Text),
                                  merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                                  orderId :: (B.C f Kernel.Prelude.Text),
                                  paymentGateway :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                  paymentMethod :: (B.C f (Kernel.Prelude.Maybe Lib.Finance.Domain.Types.PgPaymentSettlementReport.PaymentMethod)),
                                  paymentMethodSubType :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                  pgApprovalCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                  pgBaseFee :: (B.C f Kernel.Types.Common.HighPrecMoney),
                                  pgTax :: (B.C f Kernel.Types.Common.HighPrecMoney),
                                  rawData :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
                                  reconMessage :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                  reconStatus :: (B.C f Lib.Finance.Domain.Types.PgPaymentSettlementReport.ReconStatus),
                                  referenceId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                  referenceType :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                  refundAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                                  refundArn :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                  refundBaseFee :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                                  refundDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                                  refundId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                  refundMethod :: (B.C f (Kernel.Prelude.Maybe Lib.Finance.Domain.Types.PgPaymentSettlementReport.PaymentMethod)),
                                  refundReasonCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                  refundTax :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                                  rrn :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                  settlementAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                                  settlementDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                                  settlementId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                  settlementMode :: (B.C f (Kernel.Prelude.Maybe Lib.Finance.Domain.Types.PgPaymentSettlementReport.SettlementMode)),
                                  settlementType :: (B.C f (Kernel.Prelude.Maybe Lib.Finance.Domain.Types.PgPaymentSettlementReport.SettlementType)),
                                  txnAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                                  txnDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
                                  txnId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                  txnStatus :: (B.C f Lib.Finance.Domain.Types.PgPaymentSettlementReport.TxnStatus),
                                  txnType :: (B.C f Lib.Finance.Domain.Types.PgPaymentSettlementReport.TxnType),
                                  uniqueSplitId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                  updatedAt :: (B.C f Kernel.Prelude.UTCTime),
                                  utr :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
                                  vendorId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text))}
    deriving (Generic, B.Beamable)
instance B.Table PgPaymentSettlementReportT
    where data PrimaryKey PgPaymentSettlementReportT f = PgPaymentSettlementReportId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = PgPaymentSettlementReportId . id
type PgPaymentSettlementReport = PgPaymentSettlementReportT Identity

$(enableKVPG (''PgPaymentSettlementReportT) [('id)] [[('orderId)], [('referenceId)], [('rrn)], [('settlementId)], [('utr)]])

$(mkTableInstancesGenericSchema (''PgPaymentSettlementReportT) "pg_payment_settlement_report")

