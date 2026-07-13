{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.ProviderPlatform.Management.FinanceManagement
  ( getFinanceManagementSubscriptionPurchaseList,
    postFinanceManagementReconciliationTrigger,
    getFinanceManagementFinancePaymentSettlementList,
    getFinanceManagementFinanceWalletLedger,
    getFinanceManagementFinanceInvoiceList,
    getFinanceManagementFinanceReconciliation,
    getFinanceManagementFinancePaymentGatewayTransactionList,
    getFinanceManagementFinanceInvoicePdf,
    getFinanceManagementFinanceAuditList,
    getFinanceManagementFinanceSapJournals,
    getFinanceManagementFinanceSapJournalsTransactions,
  )
where

import qualified API.Client.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.FinanceManagement
import qualified Dashboard.Common
import qualified "beckn-spec" Domain.Types.Invoice
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types
import qualified Lib.Finance.Domain.Types.AuditEntry
import qualified Lib.Finance.Domain.Types.Invoice
import qualified Lib.Finance.Domain.Types.ReconciliationSummary
import qualified Lib.Finance.Domain.Types.SapJournalEntry
import qualified Lib.Finance.Invoice.PdfService
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getFinanceManagementSubscriptionPurchaseList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.FinanceManagement.SubscriptionPurchaseStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.SubscriptionPurchaseListRes)
getFinanceManagementSubscriptionPurchaseList merchantShortId opCity apiTokenInfo amountMax amountMin driverId fleetOperatorId from limit offset serviceName status subscriptionId to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.getFinanceManagementSubscriptionPurchaseList) amountMax amountMin driverId fleetOperatorId from limit offset serviceName status subscriptionId to

postFinanceManagementReconciliationTrigger :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.FinanceManagement.ReconciliationTriggerReq -> Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.ReconciliationTriggerRes)
postFinanceManagementReconciliationTrigger merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.postFinanceManagementReconciliationTrigger) req)

getFinanceManagementFinancePaymentSettlementList ::
  ( Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
    Kernel.Types.Beckn.Context.City ->
    ApiTokenInfo ->
    Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> -- from
    Kernel.Prelude.Maybe Kernel.Prelude.Int -> -- limit
    Kernel.Prelude.Maybe Kernel.Prelude.Int -> -- offset
    Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.PaymentOrder) -> -- paymentOrderShortId
    Kernel.Prelude.Maybe Kernel.Prelude.Text -> -- pgApprovalCode
    Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.FinanceManagement.PgGateway -> -- pgGateway
    Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> -- settlementAmountMax
    Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> -- settlementAmountMin
    Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> -- settlementFrom
    Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.PGPaymentSettlementReport) -> -- settlementId
    Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> -- settlementTo
    Kernel.Prelude.Maybe Kernel.Prelude.Text -> -- settlementUtr
    Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.SubscriptionPurchase) -> -- subscriptionPurchaseId
    Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> -- to
    Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.PaymentSettlementListRes
  )
getFinanceManagementFinancePaymentSettlementList merchantShortId opCity apiTokenInfo from limit offset paymentOrderShortId pgApprovalCode pgGateway settlementAmountMax settlementAmountMin settlementFrom settlementId settlementTo settlementUtr subscriptionPurchaseId to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.getFinanceManagementFinancePaymentSettlementList) from limit offset paymentOrderShortId pgApprovalCode pgGateway settlementAmountMax settlementAmountMin settlementFrom settlementId settlementTo settlementUtr subscriptionPurchaseId to

getFinanceManagementFinanceWalletLedger :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.SubscriptionPurchase) -> Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.WalletLedgerRes)
getFinanceManagementFinanceWalletLedger merchantShortId opCity apiTokenInfo limit offset driverId fleetOperatorId concernedIndividualId from to sourceType subscriptionId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.getFinanceManagementFinanceWalletLedger) limit offset driverId fleetOperatorId concernedIndividualId from to sourceType subscriptionId

getFinanceManagementFinanceInvoiceList ::
  ( Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
    Kernel.Types.Beckn.Context.City ->
    ApiTokenInfo ->
    Kernel.Prelude.Maybe Kernel.Prelude.Text -> -- fleetOwnerOrDriverId
    Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> -- from
    Kernel.Prelude.Maybe Kernel.Prelude.Text -> -- invoiceId
    Kernel.Prelude.Maybe Kernel.Prelude.Text -> -- invoiceNumber
    Kernel.Prelude.Maybe (Domain.Types.Invoice.InvoiceType) -> -- invoiceType
    Kernel.Prelude.Maybe (Domain.Types.Invoice.IssuedToType) -> -- issuedToType
    Kernel.Prelude.Maybe [Domain.Types.Invoice.IssuedToType] -> -- issuedToTypes
    Kernel.Prelude.Maybe Kernel.Prelude.Int -> -- limit
    Kernel.Prelude.Maybe Kernel.Prelude.Int -> -- offset
    Kernel.Prelude.Maybe (Lib.Finance.Domain.Types.Invoice.InvoiceStatus) -> -- status
    Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> -- to
    Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.InvoiceListRes
  )
getFinanceManagementFinanceInvoiceList merchantShortId opCity apiTokenInfo fleetOwnerOrDriverId from invoiceId invoiceNumber invoiceType issuedToType issuedToTypes limit offset status to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.getFinanceManagementFinanceInvoiceList) fleetOwnerOrDriverId from invoiceId invoiceNumber invoiceType issuedToType issuedToTypes limit offset status to

getFinanceManagementFinanceReconciliation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationType -> Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.ReconciliationRes)
getFinanceManagementFinanceReconciliation merchantShortId opCity apiTokenInfo fromDate limit offset toDate reconciliationType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.getFinanceManagementFinanceReconciliation) fromDate limit offset toDate reconciliationType

getFinanceManagementFinancePaymentGatewayTransactionList ::
  ( Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
    Kernel.Types.Beckn.Context.City ->
    ApiTokenInfo ->
    Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> -- from
    Kernel.Prelude.Maybe Kernel.Prelude.Int -> -- limit
    Kernel.Prelude.Maybe Kernel.Prelude.Int -> -- offset
    Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.FinanceManagement.PaymentModeFilter -> -- paymentMode
    Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.PaymentOrder) -> -- paymentOrderId
    Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.FinanceManagement.PaymentStatusFilter -> -- paymentStatus
    Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.FinanceManagement.PgGateway -> -- pgGateway
    Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.SubscriptionPurchase) -> -- subscriptionId
    Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> -- to
    Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> -- txnAmountMax
    Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> -- txnAmountMin
    Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.PaymentTransactionReportListRes
  )
getFinanceManagementFinancePaymentGatewayTransactionList merchantShortId opCity apiTokenInfo from limit offset paymentMode paymentOrderId paymentStatus pgGateway subscriptionId to txnAmountMax txnAmountMin = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.getFinanceManagementFinancePaymentGatewayTransactionList) from limit offset paymentMode paymentOrderId paymentStatus pgGateway subscriptionId to txnAmountMax txnAmountMin

getFinanceManagementFinanceInvoicePdf :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Domain.Types.Invoice.InvoiceType) -> Kernel.Prelude.Maybe (Domain.Types.Invoice.IssuedToType) -> Kernel.Prelude.Maybe [Domain.Types.Invoice.IssuedToType] -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Lib.Finance.Domain.Types.Invoice.InvoiceStatus) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.FinanceInvoicePdfResp)
getFinanceManagementFinanceInvoicePdf merchantShortId opCity apiTokenInfo fleetOwnerOrDriverId from invoiceId invoiceNumber invoiceType issuedToType issuedToTypes language limit offset status to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.getFinanceManagementFinanceInvoicePdf) fleetOwnerOrDriverId from invoiceId invoiceNumber invoiceType issuedToType issuedToTypes language limit offset status to

getFinanceManagementFinanceAuditList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.AuditEntry.AuditEntityType -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.AuditEntry.AuditAction -> Kernel.Prelude.Maybe Lib.Finance.Core.Types.ActorType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.AuditListRes)
getFinanceManagementFinanceAuditList merchantShortId opCity apiTokenInfo limit offset from to entityType action actorType actorId entityId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.getFinanceManagementFinanceAuditList) limit offset from to entityType action actorType actorId entityId

getFinanceManagementFinanceSapJournals :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.SapJournalEntry.JournalEntryStatus -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.SapJournalEntry.TransactionType -> Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.SapJournalListRes)
getFinanceManagementFinanceSapJournals merchantShortId opCity apiTokenInfo batchId belnr dateFrom dateTo glNumber limit offset status transactionType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.getFinanceManagementFinanceSapJournals) batchId belnr dateFrom dateTo glNumber limit offset status transactionType

getFinanceManagementFinanceSapJournalsTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Lib.Finance.Domain.Types.SapJournalEntry.JournalEntryStatus -> Lib.Finance.Domain.Types.SapJournalEntry.TransactionType -> Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.SapJournalTransactionsRes)
getFinanceManagementFinanceSapJournalsTransactions merchantShortId opCity apiTokenInfo fromTime limit offset subscriptionId toTime periodEndTime periodStartTime status transactionType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.getFinanceManagementFinanceSapJournalsTransactions) fromTime limit offset subscriptionId toTime periodEndTime periodStartTime status transactionType
