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

data SubscriptionPurchaseListItem = SubscriptionPurchaseListItem
  { subscriptionPurchaseId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    planName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    planType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    subscriptionStartDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    subscriptionEndDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    subscriptionStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fleetOperatorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    subscriptionAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    gstRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    gstAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    totalSubscriptionValue :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    totalEntitledValue :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    utilizedValue :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    remainingValue :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    linkedRideIds :: [Kernel.Prelude.Text],
    invoiceIds :: [Kernel.Prelude.Text],
    createdAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SubscriptionPurchaseListRes = SubscriptionPurchaseListRes {totalItems :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary, subscriptions :: [SubscriptionPurchaseListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("financeManagement" :> (GetFinanceManagementSubscriptionPurchaseList :<|> GetFinanceManagementFleetOperatorFinanceList :<|> GetFinanceManagementInvoiceList :<|> GetFinanceManagementReconciliation :<|> PostFinanceManagementReconciliationTrigger))

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
           '[JSON]
           SubscriptionPurchaseListRes
  )

type GetFinanceManagementFleetOperatorFinanceList =
  ( "fleetOperator" :> "finance" :> "list" :> QueryParam "fleetOperatorId" Kernel.Prelude.Text
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "settlementStatus"
           Kernel.Prelude.Text
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           FleetOperatorListRes
  )

type GetFinanceManagementInvoiceList =
  ( "invoice" :> "list" :> QueryParam "from" Kernel.Prelude.UTCTime :> QueryParam "invoiceId" Kernel.Prelude.Text
      :> QueryParam
           "invoiceType"
           Kernel.Prelude.Text
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "status"
           Kernel.Prelude.Text
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           InvoiceListRes
  )

type GetFinanceManagementReconciliation =
  ( "reconciliation" :> QueryParam "fromDate" Kernel.Prelude.UTCTime :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "reconciliationType" Kernel.Prelude.Text
      :> QueryParam "toDate" Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           ReconciliationRes
  )

type PostFinanceManagementReconciliationTrigger = ("reconciliation" :> "trigger" :> ReqBody '[JSON] ReconciliationTriggerReq :> Post '[JSON] ReconciliationTriggerRes)

data FinanceManagementAPIs = FinanceManagementAPIs
  { getFinanceManagementSubscriptionPurchaseList :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient SubscriptionPurchaseListRes,
    getFinanceManagementFleetOperatorFinanceList :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient FleetOperatorListRes,
    getFinanceManagementInvoiceList :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient InvoiceListRes,
    getFinanceManagementReconciliation :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient ReconciliationRes,
    postFinanceManagementReconciliationTrigger :: ReconciliationTriggerReq -> EulerHS.Types.EulerClient ReconciliationTriggerRes
  }

mkFinanceManagementAPIs :: (Client EulerHS.Types.EulerClient API -> FinanceManagementAPIs)
mkFinanceManagementAPIs financeManagementClient = (FinanceManagementAPIs {..})
  where
    getFinanceManagementSubscriptionPurchaseList :<|> getFinanceManagementFleetOperatorFinanceList :<|> getFinanceManagementInvoiceList :<|> getFinanceManagementReconciliation :<|> postFinanceManagementReconciliationTrigger = financeManagementClient

data FinanceManagementUserActionType
  = GET_FINANCE_MANAGEMENT_SUBSCRIPTION_PURCHASE_LIST
  | GET_FINANCE_MANAGEMENT_FLEET_OPERATOR_FINANCE_LIST
  | GET_FINANCE_MANAGEMENT_INVOICE_LIST
  | GET_FINANCE_MANAGEMENT_RECONCILIATION
  | POST_FINANCE_MANAGEMENT_RECONCILIATION_TRIGGER
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''FinanceManagementUserActionType])
