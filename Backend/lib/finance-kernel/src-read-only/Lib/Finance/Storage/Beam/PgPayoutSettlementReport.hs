{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.PgPayoutSettlementReport where

import qualified Data.Aeson
import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.PgPaymentSettlementReport
import qualified Lib.Finance.Domain.Types.PgPayoutSettlementReport

data PgPayoutSettlementReportT f = PgPayoutSettlementReportT
  { bankName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    beneficiaryAccountNumber :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    beneficiaryIfsc :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    beneficiaryType :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    currency :: (B.C f Kernel.Types.Common.Currency),
    fulfillmentAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    fulfillmentDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    fulfillmentId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    fulfillmentInstrumentType :: (B.C f (Kernel.Prelude.Maybe Lib.Finance.Domain.Types.PgPayoutSettlementReport.FulfillmentInstrument)),
    fulfillmentMethod :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    fulfillmentResponseCode :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    fulfillmentResponseMessage :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    fulfillmentStatus :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    orderId :: (B.C f Kernel.Prelude.Text),
    paymentGateway :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    payoutCustomerId :: (B.C f Kernel.Prelude.Text),
    payoutRequestId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    rawData :: (B.C f (Kernel.Prelude.Maybe Data.Aeson.Value)),
    reconMessage :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    reconStatus :: (B.C f Lib.Finance.Domain.Types.PgPaymentSettlementReport.ReconStatus),
    referenceType :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    rrn :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    settlementAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    settlementDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    settlementId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    settlementMode :: (B.C f (Kernel.Prelude.Maybe Lib.Finance.Domain.Types.PgPayoutSettlementReport.SettlementMode)),
    settlementType :: (B.C f (Kernel.Prelude.Maybe Lib.Finance.Domain.Types.PgPayoutSettlementReport.SettlementType)),
    txnAmount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    txnDate :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    txnId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    txnStatus :: (B.C f Lib.Finance.Domain.Types.PgPayoutSettlementReport.TxnStatus),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    utr :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text))
  }
  deriving (Generic, B.Beamable)

instance B.Table PgPayoutSettlementReportT where
  data PrimaryKey PgPayoutSettlementReportT f = PgPayoutSettlementReportId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = PgPayoutSettlementReportId . id

type PgPayoutSettlementReport = PgPayoutSettlementReportT Identity

$(enableKVPG (''PgPayoutSettlementReportT) [('id)] [[('orderId)], [('payoutCustomerId)], [('payoutRequestId)], [('rrn)], [('settlementId)]])

$(mkTableInstancesGenericSchema (''PgPayoutSettlementReportT) "pg_payout_settlement_report")
