{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.FinanceManagement where

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
import qualified Lib.Finance.Domain.Types.Invoice
import qualified Lib.Finance.Domain.Types.ReconciliationSummary
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
  { invoiceId :: Kernel.Prelude.Text,
    invoiceNumber :: Kernel.Prelude.Text,
    invoiceType :: Lib.Finance.Domain.Types.Invoice.InvoiceType,
    invoiceDate :: Kernel.Prelude.UTCTime,
    invoiceStatus :: Lib.Finance.Domain.Types.Invoice.InvoiceStatus,
    counterpartyType :: Kernel.Prelude.Text,
    counterpartyId :: Kernel.Prelude.Text,
    taxableValue :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    gstRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    gstAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    totalInvoiceValue :: Kernel.Types.Common.HighPrecMoney,
    tdsReference :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    irn :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    qrCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rideId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    subscriptionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    cgstAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    sgstAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    supplierName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    supplierAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    supplierGstin :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    supplierTaxNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    issuedToName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    issuedToAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    issuedByName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    issuedByAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gstinOfParty :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sacCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentMethod :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    taxableValueOfServiceSupplied :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    lineItems :: Data.Aeson.Value,
    generatedAt :: Kernel.Prelude.UTCTime,
    taxRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    issuedToTaxNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    issuedByTaxNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data InvoiceListRes = InvoiceListRes {totalItems :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary, invoices :: [InvoiceListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LinkedRideItem = LinkedRideItem {rideId :: Kernel.Prelude.Text, bookingId :: Kernel.Prelude.Text, rideCreatedAt :: Kernel.Prelude.UTCTime, rideSubscriptionDebitAmount :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PaymentModeFilter
  = UPI
  | Card
  | NetBanking
  | Wallet
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data PaymentSettlementListItem = PaymentSettlementListItem
  { subscriptionPurchaseId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantRefNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pgApprovalCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pgOrderId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    transactionDateAndTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    transactionType :: Kernel.Prelude.Maybe TransactionTypeFilter,
    chargedAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    paymentStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pgFees :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    gstOnPgFees :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    netAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    merchantId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentMode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverMobileNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    driverEmailId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pgName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    chargebackAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    chargebackId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    chargebackReasonCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    chargebackStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    representmentStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    settlementCycle :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    utr :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rrnNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reconciliationStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    reconciliationDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    differenceAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PaymentSettlementListRes = PaymentSettlementListRes {totalItems :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary, settlements :: [PaymentSettlementListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PaymentStatusFilter
  = Success
  | Failed
  | Pending
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data PaymentTransactionReportItem = PaymentTransactionReportItem
  { gatewayTransactionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantOrderId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    referenceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    correlationId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    parentTransactionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    transactionType :: Kernel.Prelude.Maybe TransactionTypeFilter,
    transactionStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    failureReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    errorCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    transactionInitiationDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    paymentPageOpenDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    paymentGatewayOpenDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    refundInitiationDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    refundCompletionDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    transactionAmountGross :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    netAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    gatewayCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    gstOnCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    tdsAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    withholdingAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    refundAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    exchangeRate :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    paymentMode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentSubMode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    upiId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    upiAppName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    cardType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    cardNetwork :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    maskedCardNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    issuerBank :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    walletProvider :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payerType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payerName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payerMobile :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    payerEmail :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    subscriptionId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fleetOwnerId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    city :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    operatingLocation :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    refundId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    refundReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    refundMode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    refundStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ipAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pgName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pgMerchantId :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PaymentTransactionReportListRes = PaymentTransactionReportListRes {totalItems :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary, transactions :: [PaymentTransactionReportItem]}
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

data PgGateway
  = Juspay
  | BillDesk
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data ReconciliationEntry = ReconciliationEntry
  { bookingId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    dcoId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    expectedValue :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    actualValue :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    variance :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    reconStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    mismatchReason :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    timestamp :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    financeComponent :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sourceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    targetId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    settlementMode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    transactionDate :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    rrn :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data ReconciliationRes = ReconciliationRes {summary :: ReconciliationSummary, exceptions :: [ReconciliationEntry], completed :: [ReconciliationEntry]}
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
    linkedRides :: [LinkedRideItem],
    createdAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SubscriptionPurchaseListRes = SubscriptionPurchaseListRes {totalItems :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary, subscriptions :: [SubscriptionPurchaseListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data SubscriptionPurchaseStatus
  = SubscriptionPending
  | SubscriptionActive
  | SubscriptionExpired
  | SubscriptionFailed
  | SubscriptionExhausted
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

data TransactionTypeFilter
  = Order
  | Refund
  | Chargeback
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, Kernel.Prelude.ToParamSchema)

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

type API = ("financeManagement" :> (GetFinanceManagementSubscriptionPurchaseList :<|> GetFinanceManagementFinanceInvoiceList :<|> GetFinanceManagementFinanceReconciliation :<|> GetFinanceManagementFinancePaymentSettlementList :<|> GetFinanceManagementFinancePaymentGatewayTransactionList :<|> GetFinanceManagementFinanceWalletLedger :<|> PostFinanceManagementReconciliationTrigger))

type GetFinanceManagementSubscriptionPurchaseList =
  ( "subscriptionPurchase" :> "list" :> QueryParam "amountMax" Kernel.Types.Common.HighPrecMoney
      :> QueryParam
           "amountMin"
           Kernel.Types.Common.HighPrecMoney
      :> QueryParam "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver)
      :> QueryParam
           "fleetOwnerId"
           Kernel.Prelude.Text
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
           SubscriptionPurchaseStatus
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

type GetFinanceManagementFinanceInvoiceList =
  ( "finance" :> "invoice" :> "list" :> QueryParam "fleetOwnerOrDriverId" Kernel.Prelude.Text
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam "invoiceId" Kernel.Prelude.Text
      :> QueryParam "invoiceNumber" Kernel.Prelude.Text
      :> QueryParam
           "invoiceType"
           Lib.Finance.Domain.Types.Invoice.InvoiceType
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "status"
           Lib.Finance.Domain.Types.Invoice.InvoiceStatus
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           InvoiceListRes
  )

type GetFinanceManagementFinanceReconciliation =
  ( "finance" :> "reconciliation" :> QueryParam "fromDate" Kernel.Prelude.UTCTime :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam "toDate" Kernel.Prelude.UTCTime
      :> MandatoryQueryParam
           "reconciliationType"
           Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationType
      :> Get
           '[JSON]
           ReconciliationRes
  )

type GetFinanceManagementFinancePaymentSettlementList =
  ( "finance" :> "payment" :> "settlement" :> "list" :> QueryParam "from" Kernel.Prelude.UTCTime
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "paymentOrderShortId"
           (Kernel.Types.Id.ShortId Dashboard.Common.PaymentOrder)
      :> QueryParam
           "pgApprovalCode"
           Kernel.Prelude.Text
      :> QueryParam
           "pgGateway"
           PgGateway
      :> QueryParam
           "settlementAmountMax"
           Kernel.Types.Common.HighPrecMoney
      :> QueryParam
           "settlementAmountMin"
           Kernel.Types.Common.HighPrecMoney
      :> QueryParam
           "settlementFrom"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "settlementId"
           (Kernel.Types.Id.Id Dashboard.Common.PGPaymentSettlementReport)
      :> QueryParam
           "settlementTo"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "settlementUtr"
           Kernel.Prelude.Text
      :> QueryParam
           "subscriptionPurchaseId"
           (Kernel.Types.Id.Id Dashboard.Common.SubscriptionPurchase)
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> Get
           '[JSON]
           PaymentSettlementListRes
  )

type GetFinanceManagementFinancePaymentGatewayTransactionList =
  ( "finance" :> "payment" :> "gateway" :> "transaction" :> "list" :> QueryParam "from" Kernel.Prelude.UTCTime
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> QueryParam
           "paymentMode"
           PaymentModeFilter
      :> QueryParam
           "paymentOrderId"
           (Kernel.Types.Id.ShortId Dashboard.Common.PaymentOrder)
      :> QueryParam
           "paymentStatus"
           PaymentStatusFilter
      :> QueryParam
           "pgGateway"
           PgGateway
      :> QueryParam
           "subscriptionId"
           (Kernel.Types.Id.Id Dashboard.Common.SubscriptionPurchase)
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "txnAmountMax"
           Kernel.Types.Common.HighPrecMoney
      :> QueryParam
           "txnAmountMin"
           Kernel.Types.Common.HighPrecMoney
      :> Get
           '[JSON]
           PaymentTransactionReportListRes
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
           '[JSON]
           WalletLedgerRes
  )

type PostFinanceManagementReconciliationTrigger = ("reconciliation" :> "trigger" :> ReqBody '[JSON] ReconciliationTriggerReq :> Post '[JSON] ReconciliationTriggerRes)

data FinanceManagementAPIs = FinanceManagementAPIs
  { getFinanceManagementSubscriptionPurchaseList :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe SubscriptionPurchaseStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient SubscriptionPurchaseListRes,
    getFinanceManagementFinanceInvoiceList :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Invoice.InvoiceType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Invoice.InvoiceStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient InvoiceListRes,
    getFinanceManagementFinanceReconciliation :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationType -> EulerHS.Types.EulerClient ReconciliationRes,
    getFinanceManagementFinancePaymentSettlementList :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.PaymentOrder) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe PgGateway -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.PGPaymentSettlementReport) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.SubscriptionPurchase) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> EulerHS.Types.EulerClient PaymentSettlementListRes,
    getFinanceManagementFinancePaymentGatewayTransactionList :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe PaymentModeFilter -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.PaymentOrder) -> Kernel.Prelude.Maybe PaymentStatusFilter -> Kernel.Prelude.Maybe PgGateway -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.SubscriptionPurchase) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> EulerHS.Types.EulerClient PaymentTransactionReportListRes,
    getFinanceManagementFinanceWalletLedger :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient WalletLedgerRes,
    postFinanceManagementReconciliationTrigger :: ReconciliationTriggerReq -> EulerHS.Types.EulerClient ReconciliationTriggerRes
  }

mkFinanceManagementAPIs :: (Client EulerHS.Types.EulerClient API -> FinanceManagementAPIs)
mkFinanceManagementAPIs financeManagementClient = (FinanceManagementAPIs {..})
  where
    getFinanceManagementSubscriptionPurchaseList :<|> getFinanceManagementFinanceInvoiceList :<|> getFinanceManagementFinanceReconciliation :<|> getFinanceManagementFinancePaymentSettlementList :<|> getFinanceManagementFinancePaymentGatewayTransactionList :<|> getFinanceManagementFinanceWalletLedger :<|> postFinanceManagementReconciliationTrigger = financeManagementClient

data FinanceManagementUserActionType
  = GET_FINANCE_MANAGEMENT_SUBSCRIPTION_PURCHASE_LIST
  | GET_FINANCE_MANAGEMENT_FINANCE_INVOICE_LIST
  | GET_FINANCE_MANAGEMENT_FINANCE_RECONCILIATION
  | GET_FINANCE_MANAGEMENT_FINANCE_PAYMENT_SETTLEMENT_LIST
  | GET_FINANCE_MANAGEMENT_FINANCE_PAYMENT_GATEWAY_TRANSACTION_LIST
  | GET_FINANCE_MANAGEMENT_FINANCE_WALLET_LEDGER
  | POST_FINANCE_MANAGEMENT_RECONCILIATION_TRIGGER
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''PaymentModeFilter)

$(mkHttpInstancesForEnum ''PaymentStatusFilter)

$(mkHttpInstancesForEnum ''PgGateway)

$(mkHttpInstancesForEnum ''SubscriptionPurchaseStatus)

$(mkHttpInstancesForEnum ''TransactionTypeFilter)

$(Data.Singletons.TH.genSingletons [''FinanceManagementUserActionType])
