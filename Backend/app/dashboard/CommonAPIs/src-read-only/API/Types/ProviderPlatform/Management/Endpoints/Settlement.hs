{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.Settlement where

import qualified Dashboard.Common
import Data.Aeson
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import Servant
import Servant.Client

-- ---------------------------------------------------------------------------
-- Enums
-- ---------------------------------------------------------------------------

data SettlementStatusFilter = AllSettlements | Settled | Pending | Failed | Disputed
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''SettlementStatusFilter)

data ChargebackStatusFilter = AllChargebacks | ChargebackOpen | ChargebackEvidenceSubmitted | ChargebackWon | ChargebackLost | ChargebackExpired
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''ChargebackStatusFilter)

data TrendGranularity = Daily | Weekly | Monthly
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkHttpInstancesForEnum ''TrendGranularity)

data ChargebackAction = AcceptChargeback | RejectChargeback | SubmitEvidence
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''ChargebackAction)

-- ---------------------------------------------------------------------------
-- Response / Request types
-- ---------------------------------------------------------------------------

data SettlementSummaryRes = SettlementSummaryRes
  { totalSettledAmount :: Kernel.Types.Common.HighPrecMoney,
    totalPendingAmount :: Kernel.Types.Common.HighPrecMoney,
    totalFailedAmount :: Kernel.Types.Common.HighPrecMoney,
    totalDisputedAmount :: Kernel.Types.Common.HighPrecMoney,
    totalSettlementCount :: Kernel.Prelude.Int,
    pendingCount :: Kernel.Prelude.Int,
    failedCount :: Kernel.Prelude.Int,
    disputedCount :: Kernel.Prelude.Int,
    settlementRate :: Kernel.Prelude.Text,
    gatewayBreakdown :: [GatewaySettlementSummary]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data GatewaySettlementSummary = GatewaySettlementSummary
  { gateway :: Kernel.Prelude.Text,
    settledAmount :: Kernel.Types.Common.HighPrecMoney,
    pendingAmount :: Kernel.Types.Common.HighPrecMoney,
    failedAmount :: Kernel.Types.Common.HighPrecMoney,
    transactionCount :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SettlementListRes = SettlementListRes
  { totalItems :: Kernel.Prelude.Int,
    summary :: Dashboard.Common.Summary,
    settlements :: [SettlementListItem]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SettlementListItem = SettlementListItem
  { settlementReportId :: Kernel.Prelude.Text,
    orderId :: Kernel.Prelude.Text,
    txnId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rrn :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    txnType :: Kernel.Prelude.Text,
    txnStatus :: Kernel.Prelude.Text,
    txnAmount :: Kernel.Types.Common.HighPrecMoney,
    settlementAmount :: Kernel.Types.Common.HighPrecMoney,
    pgBaseFee :: Kernel.Types.Common.HighPrecMoney,
    pgTax :: Kernel.Types.Common.HighPrecMoney,
    paymentGateway :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentMethod :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    settlementStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reconStatus :: Kernel.Prelude.Text,
    hasChargeback :: Kernel.Prelude.Bool,
    chargebackAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    txnDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SettlementDetailsRes = SettlementDetailsRes
  { settlementReport :: SettlementReportDetail,
    linkedChargebacks :: [ChargebackItem],
    reconEntries :: [SettlementReconEntry]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SettlementReportDetail = SettlementReportDetail
  { id :: Kernel.Prelude.Text,
    orderId :: Kernel.Prelude.Text,
    txnId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rrn :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    utr :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    txnType :: Kernel.Prelude.Text,
    txnStatus :: Kernel.Prelude.Text,
    txnDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    txnAmount :: Kernel.Types.Common.HighPrecMoney,
    pgBaseFee :: Kernel.Types.Common.HighPrecMoney,
    pgTax :: Kernel.Types.Common.HighPrecMoney,
    settlementAmount :: Kernel.Types.Common.HighPrecMoney,
    paymentGateway :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentMethod :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentMethodSubType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementMode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    referenceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    referenceType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    disputeId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    disputeType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reconStatus :: Kernel.Prelude.Text,
    reconMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SettlementReconEntry = SettlementReconEntry
  { reconEntryId :: Kernel.Prelude.Text,
    bookingId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    expectedValue :: Kernel.Types.Common.HighPrecMoney,
    actualValue :: Kernel.Types.Common.HighPrecMoney,
    variance :: Kernel.Types.Common.HighPrecMoney,
    reconStatus :: Kernel.Prelude.Text,
    mismatchReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ChargebackListRes = ChargebackListRes
  { totalItems :: Kernel.Prelude.Int,
    summary :: Dashboard.Common.Summary,
    chargebacks :: [ChargebackItem],
    statusCounts :: ChargebackStatusCounts
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ChargebackStatusCounts = ChargebackStatusCounts
  { openCount :: Kernel.Prelude.Int,
    evidenceSubmittedCount :: Kernel.Prelude.Int,
    wonCount :: Kernel.Prelude.Int,
    lostCount :: Kernel.Prelude.Int,
    expiredCount :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ChargebackItem = ChargebackItem
  { chargebackId :: Kernel.Prelude.Text,
    settlementReportId :: Kernel.Prelude.Text,
    transactionId :: Kernel.Prelude.Text,
    chargebackReasonCode :: Kernel.Prelude.Text,
    chargebackAmount :: Kernel.Types.Common.HighPrecMoney,
    chargebackStatus :: Kernel.Prelude.Text,
    responseDeadline :: Kernel.Prelude.UTCTime,
    evidenceUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    adminNotes :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ChargebackRespondReq = ChargebackRespondReq
  { action :: ChargebackAction,
    evidenceUrl :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    adminNotes :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ChargebackRespondRes = ChargebackRespondRes
  { success :: Kernel.Prelude.Bool,
    message :: Kernel.Prelude.Text,
    updatedStatus :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SettlementTrendRes = SettlementTrendRes
  { trendData :: [SettlementTrendPoint],
    totalSettled :: Kernel.Types.Common.HighPrecMoney,
    totalPending :: Kernel.Types.Common.HighPrecMoney,
    periodStart :: Kernel.Prelude.UTCTime,
    periodEnd :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SettlementTrendPoint = SettlementTrendPoint
  { date :: Kernel.Prelude.UTCTime,
    settledAmount :: Kernel.Types.Common.HighPrecMoney,
    pendingAmount :: Kernel.Types.Common.HighPrecMoney,
    failedAmount :: Kernel.Types.Common.HighPrecMoney,
    disputedAmount :: Kernel.Types.Common.HighPrecMoney,
    transactionCount :: Kernel.Prelude.Int,
    settlementCount :: Kernel.Prelude.Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- ---------------------------------------------------------------------------
-- HideSecrets instances
-- ---------------------------------------------------------------------------

instance Dashboard.Common.HideSecrets ChargebackRespondReq where
  hideSecrets = Kernel.Prelude.identity
