{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.FinanceManagement
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.FinanceManagement
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.FinanceManagement
import qualified "beckn-spec" Domain.Types.Invoice
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types
import qualified Lib.Finance.Domain.Types.AuditEntry
import qualified Lib.Finance.Domain.Types.Invoice
import qualified Lib.Finance.Domain.Types.SapJournalEntry
import qualified Lib.Finance.Reconciliation.Types
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("financeManagement" :> (GetFinanceManagementSubscriptionPurchaseList :<|> GetFinanceManagementFinanceInvoicePdf :<|> GetFinanceManagementFinanceInvoiceList :<|> GetFinanceManagementFinanceAuditList :<|> GetFinanceManagementFinanceReconciliation :<|> GetFinanceManagementFinancePaymentSettlementList :<|> GetFinanceManagementFinancePaymentGatewayTransactionList :<|> GetFinanceManagementFinanceWalletLedger :<|> PostFinanceManagementReconciliationTrigger :<|> PostFinanceManagementFinanceAdjustmentSubmit :<|> GetFinanceManagementFinanceAdjustmentList :<|> PostFinanceManagementFinanceAdjustmentApprove :<|> PostFinanceManagementFinanceAdjustmentReject :<|> GetFinanceManagementFinanceSapJournals :<|> GetFinanceManagementFinanceSapJournalsTransactions :<|> PostFinanceManagementTdsReimbursementRequestSubmit :<|> PostFinanceManagementTdsReimbursementReject :<|> GetFinanceManagementTdsReimbursementStatus :<|> GetFinanceManagementTdsReimbursementList :<|> GetFinanceManagementTdsReimbursement))

type GetFinanceManagementSubscriptionPurchaseList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_SUBSCRIPTION_PURCHASE_LIST"
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementSubscriptionPurchaseList
  )

type GetFinanceManagementFinanceInvoicePdf =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_INVOICE_PDF"
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinanceInvoicePdf
  )

type GetFinanceManagementFinanceInvoiceList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_INVOICE_LIST"
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinanceInvoiceList
  )

type GetFinanceManagementFinanceAuditList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_AUDIT_LIST"
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinanceAuditList
  )

type GetFinanceManagementFinanceReconciliation =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_RECONCILIATION"
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinanceReconciliation
  )

type GetFinanceManagementFinancePaymentSettlementList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_PAYMENT_SETTLEMENT_LIST"
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinancePaymentSettlementList
  )

type GetFinanceManagementFinancePaymentGatewayTransactionList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_PAYMENT_GATEWAY_TRANSACTION_LIST"
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinancePaymentGatewayTransactionList
  )

type GetFinanceManagementFinanceWalletLedger =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_WALLET_LEDGER"
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinanceWalletLedger
  )

type PostFinanceManagementReconciliationTrigger =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/POST_FINANCE_MANAGEMENT_RECONCILIATION_TRIGGER"
      :> API.Types.ProviderPlatform.Management.FinanceManagement.PostFinanceManagementReconciliationTrigger
  )

type PostFinanceManagementFinanceAdjustmentSubmit =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/POST_FINANCE_MANAGEMENT_FINANCE_ADJUSTMENT_SUBMIT"
      :> API.Types.ProviderPlatform.Management.FinanceManagement.PostFinanceManagementFinanceAdjustmentSubmit
  )

type GetFinanceManagementFinanceAdjustmentList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_ADJUSTMENT_LIST"
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinanceAdjustmentList
  )

type PostFinanceManagementFinanceAdjustmentApprove =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/POST_FINANCE_MANAGEMENT_FINANCE_ADJUSTMENT_APPROVE"
      :> API.Types.ProviderPlatform.Management.FinanceManagement.PostFinanceManagementFinanceAdjustmentApprove
  )

type PostFinanceManagementFinanceAdjustmentReject =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/POST_FINANCE_MANAGEMENT_FINANCE_ADJUSTMENT_REJECT"
      :> API.Types.ProviderPlatform.Management.FinanceManagement.PostFinanceManagementFinanceAdjustmentReject
  )

type GetFinanceManagementFinanceSapJournals =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_SAP_JOURNALS"
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinanceSapJournals
  )

type GetFinanceManagementFinanceSapJournalsTransactions =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_SAP_JOURNALS_TRANSACTIONS"
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinanceSapJournalsTransactions
  )

type PostFinanceManagementTdsReimbursementRequestSubmit =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/POST_FINANCE_MANAGEMENT_TDS_REIMBURSEMENT_REQUEST_SUBMIT"
      :> API.Types.ProviderPlatform.Management.FinanceManagement.PostFinanceManagementTdsReimbursementRequestSubmit
  )

type PostFinanceManagementTdsReimbursementReject =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/POST_FINANCE_MANAGEMENT_TDS_REIMBURSEMENT_REJECT"
      :> API.Types.ProviderPlatform.Management.FinanceManagement.PostFinanceManagementTdsReimbursementReject
  )

type GetFinanceManagementTdsReimbursementStatus =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_TDS_REIMBURSEMENT_STATUS"
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementTdsReimbursementStatus
  )

type GetFinanceManagementTdsReimbursementList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_TDS_REIMBURSEMENT_LIST"
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementTdsReimbursementList
  )

type GetFinanceManagementTdsReimbursement =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_TDS_REIMBURSEMENT"
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementTdsReimbursement
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getFinanceManagementSubscriptionPurchaseList merchantId city :<|> getFinanceManagementFinanceInvoicePdf merchantId city :<|> getFinanceManagementFinanceInvoiceList merchantId city :<|> getFinanceManagementFinanceAuditList merchantId city :<|> getFinanceManagementFinanceReconciliation merchantId city :<|> getFinanceManagementFinancePaymentSettlementList merchantId city :<|> getFinanceManagementFinancePaymentGatewayTransactionList merchantId city :<|> getFinanceManagementFinanceWalletLedger merchantId city :<|> postFinanceManagementReconciliationTrigger merchantId city :<|> postFinanceManagementFinanceAdjustmentSubmit merchantId city :<|> getFinanceManagementFinanceAdjustmentList merchantId city :<|> postFinanceManagementFinanceAdjustmentApprove merchantId city :<|> postFinanceManagementFinanceAdjustmentReject merchantId city :<|> getFinanceManagementFinanceSapJournals merchantId city :<|> getFinanceManagementFinanceSapJournalsTransactions merchantId city :<|> postFinanceManagementTdsReimbursementRequestSubmit merchantId city :<|> postFinanceManagementTdsReimbursementReject merchantId city :<|> getFinanceManagementTdsReimbursementStatus merchantId city :<|> getFinanceManagementTdsReimbursementList merchantId city :<|> getFinanceManagementTdsReimbursement merchantId city

getFinanceManagementSubscriptionPurchaseList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecMoney) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecMoney) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.FinanceManagement.SubscriptionPurchaseStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.SubscriptionPurchaseListRes)
getFinanceManagementSubscriptionPurchaseList a14 a13 _a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementSubscriptionPurchaseList a14 a13 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getFinanceManagementFinanceInvoicePdf :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.Invoice.InvoiceType) -> Kernel.Prelude.Maybe (Domain.Types.Invoice.IssuedToType) -> Kernel.Prelude.Maybe ([Domain.Types.Invoice.IssuedToType]) -> Kernel.Prelude.Maybe (Kernel.External.Types.Language) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Lib.Finance.Domain.Types.Invoice.InvoiceStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.FinanceInvoicePdfResp)
getFinanceManagementFinanceInvoicePdf a16 a15 _a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementFinanceInvoicePdf a16 a15 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getFinanceManagementFinanceInvoiceList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.Invoice.InvoiceType) -> Kernel.Prelude.Maybe (Domain.Types.Invoice.IssuedToType) -> Kernel.Prelude.Maybe ([Domain.Types.Invoice.IssuedToType]) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Lib.Finance.Domain.Types.Invoice.InvoiceStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.InvoiceListRes)
getFinanceManagementFinanceInvoiceList a14 a13 _a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementFinanceInvoiceList a14 a13 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getFinanceManagementFinanceAuditList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Lib.Finance.Domain.Types.AuditEntry.AuditEntityType) -> Kernel.Prelude.Maybe (Lib.Finance.Domain.Types.AuditEntry.AuditAction) -> Kernel.Prelude.Maybe (Lib.Finance.Core.Types.ActorType) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.AuditListRes)
getFinanceManagementFinanceAuditList a12 a11 _a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementFinanceAuditList a12 a11 a9 a8 a7 a6 a5 a4 a3 a2 a1

getFinanceManagementFinanceReconciliation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Lib.Finance.Reconciliation.Types.Domain -> Lib.Finance.Reconciliation.Types.DataSource -> Lib.Finance.Reconciliation.Types.DataSource -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.ReconciliationRes)
getFinanceManagementFinanceReconciliation a10 a9 _a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementFinanceReconciliation a10 a9 a7 a6 a5 a4 a3 a2 a1

getFinanceManagementFinancePaymentSettlementList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.PaymentOrder) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.FinanceManagement.PgGateway) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecMoney) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecMoney) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.PGPaymentSettlementReport) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.SubscriptionPurchase) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.PaymentSettlementListRes)
getFinanceManagementFinancePaymentSettlementList a17 a16 _a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementFinancePaymentSettlementList a17 a16 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getFinanceManagementFinancePaymentGatewayTransactionList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.FinanceManagement.PaymentModeFilter) -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.PaymentOrder) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.FinanceManagement.PaymentStatusFilter) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.FinanceManagement.PgGateway) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.SubscriptionPurchase) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecMoney) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecMoney) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.PaymentTransactionReportListRes)
getFinanceManagementFinancePaymentGatewayTransactionList a14 a13 _a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementFinancePaymentGatewayTransactionList a14 a13 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getFinanceManagementFinanceWalletLedger :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.SubscriptionPurchase) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.WalletLedgerRes)
getFinanceManagementFinanceWalletLedger a12 a11 _a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementFinanceWalletLedger a12 a11 a9 a8 a7 a6 a5 a4 a3 a2 a1

postFinanceManagementReconciliationTrigger :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.FinanceManagement.ReconciliationTriggerReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.ReconciliationTriggerRes)
postFinanceManagementReconciliationTrigger a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.postFinanceManagementReconciliationTrigger a4 a3 a1

postFinanceManagementFinanceAdjustmentSubmit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.FinanceManagement.SubmitLedgerAdjustmentReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFinanceManagementFinanceAdjustmentSubmit a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.postFinanceManagementFinanceAdjustmentSubmit a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) (Tools.Auth.DashboardUserAuth.dashboardRequestorName a2) a1

getFinanceManagementFinanceAdjustmentList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.LedgerAdjustmentRequest) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.FinanceManagement.AdjustmentRequestStatus) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Person) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.FinanceManagement.AdjustmentCategory) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.FinanceManagement.AdjustmentDirection) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Person) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Person) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.LedgerAdjustmentListRes)
getFinanceManagementFinanceAdjustmentList a17 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementFinanceAdjustmentList a17 a16 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a15)

postFinanceManagementFinanceAdjustmentApprove :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.LedgerAdjustmentRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFinanceManagementFinanceAdjustmentApprove a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.postFinanceManagementFinanceAdjustmentApprove a4 a3 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) (Tools.Auth.DashboardUserAuth.dashboardRequestorName a2)

postFinanceManagementFinanceAdjustmentReject :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.LedgerAdjustmentRequest -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFinanceManagementFinanceAdjustmentReject a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.postFinanceManagementFinanceAdjustmentReject a4 a3 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) (Tools.Auth.DashboardUserAuth.dashboardRequestorName a2)

getFinanceManagementFinanceSapJournals :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Lib.Finance.Domain.Types.SapJournalEntry.JournalEntryStatus) -> Kernel.Prelude.Maybe (Lib.Finance.Domain.Types.SapJournalEntry.TransactionType) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.SapJournalListRes)
getFinanceManagementFinanceSapJournals a13 a12 _a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementFinanceSapJournals a13 a12 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getFinanceManagementFinanceSapJournalsTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Text -> Lib.Finance.Domain.Types.SapJournalEntry.TransactionType -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.SapJournalTransactionsRes)
getFinanceManagementFinanceSapJournalsTransactions a9 a8 _a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementFinanceSapJournalsTransactions a9 a8 a6 a5 a4 a3 a2 a1

postFinanceManagementTdsReimbursementRequestSubmit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.FinanceManagement.TdsReimbursementRequestSubmitReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.TdsReimbursementRequestSubmitRes)
postFinanceManagementTdsReimbursementRequestSubmit a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.postFinanceManagementTdsReimbursementRequestSubmit a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1

getFinanceManagementTdsReimbursementStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.FinanceManagement.TdsReimbursementQuarter -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.TdsReimbursementStatusRes)
getFinanceManagementTdsReimbursementStatus a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementTdsReimbursementStatus a5 a4 a1 a2 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)

getFinanceManagementTdsReimbursementList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.FinanceManagement.TdsReimbursementQuarter) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.FinanceManagement.TdsReimbursementStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.TdsReimbursementListRes)
getFinanceManagementTdsReimbursementList a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementTdsReimbursementList a13 a12 a6 a5 a8 a2 a4 a10 a9 a3 a7 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a11)

getFinanceManagementTdsReimbursement :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.FinanceTdsReimbursementRequest -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.TdsReimbursementDetailRes)
getFinanceManagementTdsReimbursement a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementTdsReimbursement a4 a3 a1

postFinanceManagementTdsReimbursementReject :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.FinanceTdsReimbursementRequest -> API.Types.ProviderPlatform.Management.FinanceManagement.TdsReimbursementRejectReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFinanceManagementTdsReimbursementReject a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.postFinanceManagementTdsReimbursementReject a5 a4 a2 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3) (Tools.Auth.DashboardUserAuth.dashboardRequestorName a3) a1
