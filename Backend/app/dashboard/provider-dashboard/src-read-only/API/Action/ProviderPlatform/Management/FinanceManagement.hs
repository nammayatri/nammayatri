{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.FinanceManagement
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.FinanceManagement
import qualified Dashboard.Common
import qualified Domain.Action.ProviderPlatform.Management.FinanceManagement
import qualified "beckn-spec" Domain.Types.Invoice
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import qualified Lib.Finance.Core.Types
import qualified Lib.Finance.Domain.Types.AuditEntry
import qualified Lib.Finance.Domain.Types.Invoice
import qualified Lib.Finance.Reconciliation.Types
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("financeManagement" :> (GetFinanceManagementSubscriptionPurchaseList :<|> GetFinanceManagementFinanceInvoicePdf :<|> GetFinanceManagementFinanceInvoiceList :<|> GetFinanceManagementFinanceAuditList :<|> GetFinanceManagementFinanceReconciliation :<|> GetFinanceManagementFinancePaymentSettlementList :<|> GetFinanceManagementFinancePaymentGatewayTransactionList :<|> GetFinanceManagementFinanceWalletLedger :<|> PostFinanceManagementReconciliationTrigger :<|> GetFinanceManagementFinanceSapJournals :<|> GetFinanceManagementFinanceSapJournalsTransactions))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getFinanceManagementSubscriptionPurchaseList merchantId city :<|> getFinanceManagementFinanceInvoicePdf merchantId city :<|> getFinanceManagementFinanceInvoiceList merchantId city :<|> getFinanceManagementFinanceAuditList merchantId city :<|> getFinanceManagementFinanceReconciliation merchantId city :<|> getFinanceManagementFinancePaymentSettlementList merchantId city :<|> getFinanceManagementFinancePaymentGatewayTransactionList merchantId city :<|> getFinanceManagementFinanceWalletLedger merchantId city :<|> postFinanceManagementReconciliationTrigger merchantId city :<|> getFinanceManagementFinanceSapJournals merchantId city :<|> getFinanceManagementFinanceSapJournalsTransactions merchantId city

type GetFinanceManagementSubscriptionPurchaseList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FINANCE_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FinanceManagement.GET_FINANCE_MANAGEMENT_SUBSCRIPTION_PURCHASE_LIST)
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementSubscriptionPurchaseList
  )

type GetFinanceManagementFinanceInvoicePdf =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FINANCE_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FinanceManagement.GET_FINANCE_MANAGEMENT_FINANCE_INVOICE_PDF)
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinanceInvoicePdf
  )

type GetFinanceManagementFinanceInvoiceList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FINANCE_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FinanceManagement.GET_FINANCE_MANAGEMENT_FINANCE_INVOICE_LIST)
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinanceInvoiceList
  )

type GetFinanceManagementFinanceAuditList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FINANCE_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FinanceManagement.GET_FINANCE_MANAGEMENT_FINANCE_AUDIT_LIST)
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinanceAuditList
  )

type GetFinanceManagementFinanceReconciliation =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FINANCE_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FinanceManagement.GET_FINANCE_MANAGEMENT_FINANCE_RECONCILIATION)
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinanceReconciliation
  )

type GetFinanceManagementFinancePaymentSettlementList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FINANCE_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FinanceManagement.GET_FINANCE_MANAGEMENT_FINANCE_PAYMENT_SETTLEMENT_LIST)
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinancePaymentSettlementList
  )

type GetFinanceManagementFinancePaymentGatewayTransactionList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FINANCE_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FinanceManagement.GET_FINANCE_MANAGEMENT_FINANCE_PAYMENT_GATEWAY_TRANSACTION_LIST)
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinancePaymentGatewayTransactionList
  )

type GetFinanceManagementFinanceWalletLedger =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FINANCE_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FinanceManagement.GET_FINANCE_MANAGEMENT_FINANCE_WALLET_LEDGER)
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinanceWalletLedger
  )

type PostFinanceManagementReconciliationTrigger =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FINANCE_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FinanceManagement.POST_FINANCE_MANAGEMENT_RECONCILIATION_TRIGGER)
      :> API.Types.ProviderPlatform.Management.FinanceManagement.PostFinanceManagementReconciliationTrigger
  )

type GetFinanceManagementFinanceSapJournals =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FINANCE_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FinanceManagement.GET_FINANCE_MANAGEMENT_FINANCE_SAP_JOURNALS)
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinanceSapJournals
  )

type GetFinanceManagementFinanceSapJournalsTransactions =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FINANCE_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FinanceManagement.GET_FINANCE_MANAGEMENT_FINANCE_SAP_JOURNALS_TRANSACTIONS)
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinanceSapJournalsTransactions
  )

getFinanceManagementSubscriptionPurchaseList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.FinanceManagement.SubscriptionPurchaseStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.SubscriptionPurchaseListRes)
getFinanceManagementSubscriptionPurchaseList merchantShortId opCity apiTokenInfo amountMax amountMin driverId fleetOwnerId from limit offset serviceName status subscriptionId to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementSubscriptionPurchaseList merchantShortId opCity apiTokenInfo amountMax amountMin driverId fleetOwnerId from limit offset serviceName status subscriptionId to

getFinanceManagementFinanceInvoicePdf :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.Invoice.InvoiceType -> Kernel.Prelude.Maybe Domain.Types.Invoice.IssuedToType -> Kernel.Prelude.Maybe [Domain.Types.Invoice.IssuedToType] -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Invoice.InvoiceStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.FinanceInvoicePdfResp)
getFinanceManagementFinanceInvoicePdf merchantShortId opCity apiTokenInfo fleetOwnerOrDriverId from invoiceId invoiceNumber invoiceType issuedToType issuedToTypes language limit offset status to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementFinanceInvoicePdf merchantShortId opCity apiTokenInfo fleetOwnerOrDriverId from invoiceId invoiceNumber invoiceType issuedToType issuedToTypes language limit offset status to

getFinanceManagementFinanceInvoiceList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.Invoice.InvoiceType -> Kernel.Prelude.Maybe Domain.Types.Invoice.IssuedToType -> Kernel.Prelude.Maybe [Domain.Types.Invoice.IssuedToType] -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Invoice.InvoiceStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.InvoiceListRes)
getFinanceManagementFinanceInvoiceList merchantShortId opCity apiTokenInfo fleetOwnerOrDriverId from invoiceId invoiceNumber invoiceType issuedToType issuedToTypes limit offset status to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementFinanceInvoiceList merchantShortId opCity apiTokenInfo fleetOwnerOrDriverId from invoiceId invoiceNumber invoiceType issuedToType issuedToTypes limit offset status to

getFinanceManagementFinanceAuditList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.AuditEntry.AuditEntityType -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.AuditEntry.AuditAction -> Kernel.Prelude.Maybe Lib.Finance.Core.Types.ActorType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.AuditListRes)
getFinanceManagementFinanceAuditList merchantShortId opCity apiTokenInfo limit offset from to entityType action actorType actorId entityId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementFinanceAuditList merchantShortId opCity apiTokenInfo limit offset from to entityType action actorType actorId entityId

getFinanceManagementFinanceReconciliation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Lib.Finance.Reconciliation.Types.Domain -> Lib.Finance.Reconciliation.Types.DataSource -> Lib.Finance.Reconciliation.Types.DataSource -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.ReconciliationRes)
getFinanceManagementFinanceReconciliation merchantShortId opCity apiTokenInfo fromDate limit offset toDate domain source target = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementFinanceReconciliation merchantShortId opCity apiTokenInfo fromDate limit offset toDate domain source target

getFinanceManagementFinancePaymentSettlementList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.PaymentOrder) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.FinanceManagement.PgGateway -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.PGPaymentSettlementReport) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.SubscriptionPurchase) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.PaymentSettlementListRes)
getFinanceManagementFinancePaymentSettlementList merchantShortId opCity apiTokenInfo from limit offset paymentOrderShortId pgApprovalCode pgGateway settlementAmountMax settlementAmountMin settlementFrom settlementId settlementTo settlementUtr subscriptionPurchaseId to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementFinancePaymentSettlementList merchantShortId opCity apiTokenInfo from limit offset paymentOrderShortId pgApprovalCode pgGateway settlementAmountMax settlementAmountMin settlementFrom settlementId settlementTo settlementUtr subscriptionPurchaseId to

getFinanceManagementFinancePaymentGatewayTransactionList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.FinanceManagement.PaymentModeFilter -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.PaymentOrder) -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.FinanceManagement.PaymentStatusFilter -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.FinanceManagement.PgGateway -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.SubscriptionPurchase) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.PaymentTransactionReportListRes)
getFinanceManagementFinancePaymentGatewayTransactionList merchantShortId opCity apiTokenInfo from limit offset paymentMode paymentOrderId paymentStatus pgGateway subscriptionId to txnAmountMax txnAmountMin = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementFinancePaymentGatewayTransactionList merchantShortId opCity apiTokenInfo from limit offset paymentMode paymentOrderId paymentStatus pgGateway subscriptionId to txnAmountMax txnAmountMin

getFinanceManagementFinanceWalletLedger :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.SubscriptionPurchase) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.WalletLedgerRes)
getFinanceManagementFinanceWalletLedger merchantShortId opCity apiTokenInfo limit offset driverId fleetOperatorId concernedIndividualId from to sourceType subscriptionId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementFinanceWalletLedger merchantShortId opCity apiTokenInfo limit offset driverId fleetOperatorId concernedIndividualId from to sourceType subscriptionId

postFinanceManagementReconciliationTrigger :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.FinanceManagement.ReconciliationTriggerReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.ReconciliationTriggerRes)
postFinanceManagementReconciliationTrigger merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.postFinanceManagementReconciliationTrigger merchantShortId opCity apiTokenInfo req

getFinanceManagementFinanceSapJournals :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.SapJournalListRes)
getFinanceManagementFinanceSapJournals merchantShortId opCity apiTokenInfo batchId belnr dateFrom dateTo limit offset status transactionType = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementFinanceSapJournals merchantShortId opCity apiTokenInfo batchId belnr dateFrom dateTo limit offset status transactionType

getFinanceManagementFinanceSapJournalsTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Kernel.Prelude.UTCTime -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.SapJournalTransactionsRes)
getFinanceManagementFinanceSapJournalsTransactions merchantShortId opCity apiTokenInfo fromTime limit offset subscriptionId toTime periodEndTime periodStartTime transactionType = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementFinanceSapJournalsTransactions merchantShortId opCity apiTokenInfo fromTime limit offset subscriptionId toTime periodEndTime periodStartTime transactionType
