{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.FinanceManagement where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Common
import Servant
import Servant.Client

data EarningsSummaryRes = EarningsSummaryRes
  { totalRideEarnings :: Kernel.Types.Common.HighPrecMoney,
    totalIncentives :: Kernel.Types.Common.HighPrecMoney,
    totalPenalties :: Kernel.Types.Common.HighPrecMoney,
    grossEarnings :: Kernel.Types.Common.HighPrecMoney,
    totalTdsDeducted :: Kernel.Types.Common.HighPrecMoney,
    totalGstDeducted :: Kernel.Types.Common.HighPrecMoney,
    netEarnings :: Kernel.Types.Common.HighPrecMoney
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOperatorListItem = FleetOperatorListItem
  { fleetOperatorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    legalName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pan :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gstin :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementPreference :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    linkedDriverIds :: [Kernel.Prelude.Text],
    totalEarnings :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    totalDeductions :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    outstandingBalance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    payoutReference :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payoutAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    payoutStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FleetOperatorListRes = FleetOperatorListRes {totalItems :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary, fleetOperators :: [FleetOperatorListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data InvoiceListItem = InvoiceListItem
  { invoiceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    invoiceNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    invoiceType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    invoiceDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    invoiceStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    counterpartyType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    counterpartyId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    taxableValue :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    gstRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    gstAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    totalInvoiceValue :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    tdsReference :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    irn :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    qrCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rideId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    subscriptionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    generatedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data InvoiceListRes = InvoiceListRes {totalItems :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary, invoices :: [InvoiceListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PaginationInfo = PaginationInfo {total :: Kernel.Prelude.Int, limit :: Kernel.Prelude.Int, offset :: Kernel.Prelude.Int, hasMore :: Kernel.Prelude.Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutListItem = PayoutListItem
  { payoutReference :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payoutAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    payoutDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    payoutStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutListRes = PayoutListRes {totalItems :: Kernel.Prelude.Int, payouts :: [PayoutListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReconciliationEntriesRes = ReconciliationEntriesRes {entries :: [ReconciliationEntry], pagination :: PaginationInfo}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReconciliationEntry = ReconciliationEntry
  { bookingId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dcoId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    expectedDsrValue :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    actualLedgerValue :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    variance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    reconStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mismatchReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    timestamp :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReconciliationRes = ReconciliationRes {summary :: ReconciliationSummary, exceptions :: [ReconciliationEntry], completed :: [ReconciliationEntry]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReconciliationStatusItem = ReconciliationStatusItem
  { reconciliationType :: Kernel.Prelude.Text,
    status :: Kernel.Prelude.Text,
    lastCompletedDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    errorMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReconciliationStatusRes = ReconciliationStatusRes {statuses :: [ReconciliationStatusItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReconciliationSummariesRes = ReconciliationSummariesRes {summaries :: [ReconciliationSummary], totalCount :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReconciliationSummary = ReconciliationSummary
  { totalDiscrepancies :: Kernel.Prelude.Int,
    matchedRecords :: Kernel.Prelude.Int,
    matchRate :: Kernel.Prelude.Text,
    sourceTotal :: Kernel.Types.Common.HighPrecMoney,
    targetTotal :: Kernel.Types.Common.HighPrecMoney,
    varianceAmount :: Kernel.Types.Common.HighPrecMoney
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReconciliationTriggerReq = ReconciliationTriggerReq {fromDate :: Kernel.Prelude.UTCTime, toDate :: Kernel.Prelude.UTCTime, reconciliationType :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReconciliationTriggerRes = ReconciliationTriggerRes {success :: Kernel.Prelude.Bool, message :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RideUsageDetail = RideUsageDetail {bookingId :: Kernel.Prelude.Text, rideId :: Kernel.Prelude.Text, rideDate :: Kernel.Prelude.UTCTime, deductedAmount :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SubscriptionPurchaseListItem = SubscriptionPurchaseListItem
  { subscriptionPurchaseId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    purchasedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    driverName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fleetOwnerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverPhoneNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fleetOwnerPhoneNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fleetOwnerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    amount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    planName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    planType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    planAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    planRideCredits :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    planStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    planValidityDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    planGeography :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    planVehicleCategory :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    subscriptionStartDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    subscriptionEndDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    subscriptionStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    baseAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    discountAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    totalAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    grossSubscriptionAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    gstRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    gstAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    totalSubscriptionAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    invoiceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    totalEntitledValue :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    utilizedValue :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    remainingValue :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    revenueRecognized :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    linkedRideIds :: [Kernel.Prelude.Text],
    linkedBookingIds :: [Kernel.Prelude.Text],
    rideUsageDetails :: [RideUsageDetail],
    createdAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SubscriptionPurchaseListRes = SubscriptionPurchaseListRes {totalItems :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary, subscriptions :: [SubscriptionPurchaseListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data WalletLedgerItem = WalletLedgerItem
  { walletTxnId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    walletTxnDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    sourceType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sourceReferenceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    creditAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    debitAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    openingBalance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    closingBalance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data WalletLedgerRes = WalletLedgerRes
  { availableWalletBalance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    lockedWalletBalance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    lastWalletUpdatedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    totalItems :: Kernel.Prelude.Int,
    ledgerEntries :: [WalletLedgerItem]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("financeManagement" :> (GetFinanceManagementSubscriptionPurchaseList :<|> GetFinanceManagementFinanceInvoiceList :<|> GetFinanceManagementFinanceReconciliation :<|> GetFinanceManagementFinancePayoutList :<|> GetFinanceManagementFinanceWalletLedger :<|> GetFinanceManagementFinanceEarningSummary :<|> PostFinanceManagementReconciliationTrigger))

type GetFinanceManagementSubscriptionPurchaseList =
  ( "subscriptionPurchase" :> "list" :> QueryParam "amountMax" Kernel.Prelude.Text
      :> QueryParam
           "amountMin"
           Kernel.Prelude.Text
      :> QueryParam "driverId" Kernel.Prelude.Text
      :> QueryParam "fleetOperatorId" Kernel.Prelude.Text
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "serviceName"
           Kernel.Prelude.Text
      :> QueryParam
           "status"
           Kernel.Prelude.Text
      :> QueryParam
           "subscriptionId"
           Kernel.Prelude.Text
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           ('[JSON])
           SubscriptionPurchaseListRes
  )

type GetFinanceManagementFinanceInvoiceList =
  ( "finance" :> "invoice" :> "list" :> QueryParam "from" Kernel.Prelude.UTCTime :> QueryParam "invoiceId" Kernel.Prelude.Text
      :> QueryParam
           "invoiceType"
           Kernel.Prelude.Text
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "status"
           Kernel.Prelude.Text
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           ('[JSON])
           InvoiceListRes
  )

type GetFinanceManagementFinanceReconciliation =
  ( "finance" :> "reconciliation" :> QueryParam "fromDate" Kernel.Prelude.UTCTime :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "reconciliationType" Kernel.Prelude.Text
      :> QueryParam
           "toDate"
           Kernel.Prelude.UTCTime
      :> Get
           ('[JSON])
           ReconciliationRes
  )

type GetFinanceManagementFinancePayoutList =
  ( "finance" :> "payout" :> "list" :> QueryParam "driverId" Kernel.Prelude.Text :> QueryParam "fleetOperatorId" Kernel.Prelude.Text
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           ('[JSON])
           PayoutListRes
  )

type GetFinanceManagementFinanceWalletLedger =
  ( "finance" :> "wallet" :> "ledger" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "driverId"
           Kernel.Prelude.Text
      :> QueryParam "fleetOperatorId" Kernel.Prelude.Text
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "sourceType"
           Kernel.Prelude.Text
      :> Get
           ('[JSON])
           WalletLedgerRes
  )

type GetFinanceManagementFinanceEarningSummary =
  ( "finance" :> "earning" :> "summary" :> QueryParam "driverId" Kernel.Prelude.Text :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam "to" Kernel.Prelude.UTCTime
      :> Get ('[JSON]) EarningsSummaryRes
  )

type PostFinanceManagementReconciliationTrigger = ("reconciliation" :> "trigger" :> ReqBody ('[JSON]) ReconciliationTriggerReq :> Post ('[JSON]) ReconciliationTriggerRes)

data FinanceManagementAPIs = FinanceManagementAPIs
  { getFinanceManagementSubscriptionPurchaseList :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> EulerHS.Types.EulerClient SubscriptionPurchaseListRes),
    getFinanceManagementFinanceInvoiceList :: (Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> EulerHS.Types.EulerClient InvoiceListRes),
    getFinanceManagementFinanceReconciliation :: (Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> EulerHS.Types.EulerClient ReconciliationRes),
    getFinanceManagementFinancePayoutList :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> EulerHS.Types.EulerClient PayoutListRes),
    getFinanceManagementFinanceWalletLedger :: (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> EulerHS.Types.EulerClient WalletLedgerRes),
    getFinanceManagementFinanceEarningSummary :: (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> EulerHS.Types.EulerClient EarningsSummaryRes),
    postFinanceManagementReconciliationTrigger :: (ReconciliationTriggerReq -> EulerHS.Types.EulerClient ReconciliationTriggerRes)
  }

mkFinanceManagementAPIs :: (Client EulerHS.Types.EulerClient API -> FinanceManagementAPIs)
mkFinanceManagementAPIs financeManagementClient = (FinanceManagementAPIs {..})
  where
    getFinanceManagementSubscriptionPurchaseList :<|> getFinanceManagementFinanceInvoiceList :<|> getFinanceManagementFinanceReconciliation :<|> getFinanceManagementFinancePayoutList :<|> getFinanceManagementFinanceWalletLedger :<|> getFinanceManagementFinanceEarningSummary :<|> postFinanceManagementReconciliationTrigger = financeManagementClient

data FinanceManagementUserActionType
  = GET_FINANCE_MANAGEMENT_SUBSCRIPTION_PURCHASE_LIST
  | GET_FINANCE_MANAGEMENT_FINANCE_INVOICE_LIST
  | GET_FINANCE_MANAGEMENT_FINANCE_RECONCILIATION
  | GET_FINANCE_MANAGEMENT_FINANCE_PAYOUT_LIST
  | GET_FINANCE_MANAGEMENT_FINANCE_WALLET_LEDGER
  | GET_FINANCE_MANAGEMENT_FINANCE_EARNING_SUMMARY
  | POST_FINANCE_MANAGEMENT_RECONCILIATION_TRIGGER
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''FinanceManagementUserActionType)])
