{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.PgPayoutSettlementReport where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Lib.Finance.Domain.Types.PgPaymentSettlementReport
import qualified Tools.Beam.UtilsTH

data PgPayoutSettlementReport = PgPayoutSettlementReport
  { bankName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    beneficiaryAccountNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    beneficiaryIfsc :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    beneficiaryType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    fulfillmentAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    fulfillmentDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    fulfillmentId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fulfillmentInstrumentType :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.PgPayoutSettlementReport.FulfillmentInstrument,
    fulfillmentMethod :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fulfillmentResponseCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fulfillmentResponseMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fulfillmentStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.PgPayoutSettlementReport.PgPayoutSettlementReport,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    orderId :: Kernel.Prelude.Text,
    paymentGateway :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payoutCustomerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payoutRequestId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rawData :: Kernel.Prelude.Maybe Data.Aeson.Value,
    reconMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reconStatus :: Lib.Finance.Domain.Types.PgPaymentSettlementReport.ReconStatus,
    referenceType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rrn :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementAmount :: Kernel.Types.Common.HighPrecMoney,
    settlementDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    settlementId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementMode :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.PgPayoutSettlementReport.SettlementMode,
    settlementType :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.PgPayoutSettlementReport.SettlementType,
    txnAmount :: Kernel.Types.Common.HighPrecMoney,
    txnDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    txnId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    txnStatus :: Lib.Finance.Domain.Types.PgPayoutSettlementReport.TxnStatus,
    updatedAt :: Kernel.Prelude.UTCTime,
    utr :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic)

data FulfillmentInstrument = IMPS | NEFT | RTGS | UPI | OTHER deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data SettlementMode = GROSS | NET | NETTING deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data SettlementType = CREDIT | DEBIT deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data TxnStatus = SUCCESS | FAILED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''FulfillmentInstrument))

$(mkHttpInstancesForEnum (''FulfillmentInstrument))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''SettlementMode))

$(mkHttpInstancesForEnum (''SettlementMode))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''SettlementType))

$(mkHttpInstancesForEnum (''SettlementType))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''TxnStatus))

$(mkHttpInstancesForEnum (''TxnStatus))
