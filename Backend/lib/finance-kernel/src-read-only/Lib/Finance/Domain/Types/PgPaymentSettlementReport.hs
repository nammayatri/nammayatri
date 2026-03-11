{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.PgPaymentSettlementReport where

import Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Kernel.Utils.TH

data PgPaymentSettlementReport = PgPaymentSettlementReport
  { bankId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    chargebackId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    chargebackReasonCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    chargebackStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    disputeId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    disputeType :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.PgPaymentSettlementReport.DisputeType,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.PgPaymentSettlementReport.PgPaymentSettlementReport,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    orderId :: Kernel.Prelude.Text,
    paymentGateway :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentMethod :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.PgPaymentSettlementReport.PaymentMethod,
    paymentMethodSubType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pgApprovalCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pgBaseFee :: Kernel.Types.Common.HighPrecMoney,
    pgTax :: Kernel.Types.Common.HighPrecMoney,
    rawData :: Kernel.Prelude.Maybe Data.Aeson.Value,
    reconMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reconStatus :: Lib.Finance.Domain.Types.PgPaymentSettlementReport.ReconStatus,
    referenceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    referenceType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    refundAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    refundArn :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    refundBaseFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    refundDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    refundId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    refundMethod :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.PgPaymentSettlementReport.PaymentMethod,
    refundReasonCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    refundTax :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    rrn :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementAmount :: Kernel.Types.Common.HighPrecMoney,
    settlementDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    settlementId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementMode :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.PgPaymentSettlementReport.SettlementMode,
    settlementType :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.PgPaymentSettlementReport.SettlementType,
    txnAmount :: Kernel.Types.Common.HighPrecMoney,
    txnDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    txnId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    txnStatus :: Lib.Finance.Domain.Types.PgPaymentSettlementReport.TxnStatus,
    txnType :: Lib.Finance.Domain.Types.PgPaymentSettlementReport.TxnType,
    uniqueSplitId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    utr :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vendorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic)

data DisputeType = FRAUD | CONSUMER | PROCESSING_ERROR | OTHER deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data PaymentMethod = UPI | CREDIT_CARD | DEBIT_CARD | NETBANKING | WALLET deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data ReconStatus = PENDING | MATCHED | MISMATCHED | MANUAL_REVIEW | IGNORED | SETTLED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data SettlementMode = GROSS | NET | NETTING deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data SettlementType = CREDIT | DEBIT deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data TxnStatus = SUCCESS | FAILED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data TxnType = ORDER | REFUND | CHARGEBACK deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''TxnType))

$(Kernel.Utils.TH.mkHttpInstancesForEnum (''TxnType))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''TxnStatus))

$(Kernel.Utils.TH.mkHttpInstancesForEnum (''TxnStatus))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''SettlementType))

$(Kernel.Utils.TH.mkHttpInstancesForEnum (''SettlementType))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''SettlementMode))

$(Kernel.Utils.TH.mkHttpInstancesForEnum (''SettlementMode))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''PaymentMethod))

$(Kernel.Utils.TH.mkHttpInstancesForEnum (''PaymentMethod))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''DisputeType))

$(Kernel.Utils.TH.mkHttpInstancesForEnum (''DisputeType))

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''ReconStatus))

$(Kernel.Utils.TH.mkHttpInstancesForEnum (''ReconStatus))
