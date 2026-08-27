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
    postFinanceManagementTdsReimbursementRequestSubmit,
    getFinanceManagementTdsReimbursementStatus,
    getFinanceManagementTdsReimbursementList,
    getFinanceManagementTdsReimbursement,
    postFinanceManagementFinanceAdjustmentSubmit,
    getFinanceManagementFinanceAdjustmentList,
    postFinanceManagementFinanceAdjustmentApprove,
    postFinanceManagementFinanceAdjustmentReject,
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
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types
import qualified Lib.Finance.Domain.Types.AuditEntry
import qualified Lib.Finance.Domain.Types.Invoice
import qualified Lib.Finance.Domain.Types.SapJournalEntry
import qualified Lib.Finance.Invoice.PdfService
import qualified Lib.Finance.Reconciliation.Types
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
    Kernel.Prelude.Maybe Domain.Types.Invoice.InvoiceType -> -- invoiceType
    Kernel.Prelude.Maybe Domain.Types.Invoice.IssuedToType -> -- issuedToType
    Kernel.Prelude.Maybe [Domain.Types.Invoice.IssuedToType] -> -- issuedToTypes
    Kernel.Prelude.Maybe Kernel.Prelude.Int -> -- limit
    Kernel.Prelude.Maybe Kernel.Prelude.Int -> -- offset
    Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Invoice.InvoiceStatus -> -- status
    Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> -- to
    Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.InvoiceListRes
  )
getFinanceManagementFinanceInvoiceList merchantShortId opCity apiTokenInfo fleetOwnerOrDriverId from invoiceId invoiceNumber invoiceType issuedToType issuedToTypes limit offset status to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.getFinanceManagementFinanceInvoiceList) fleetOwnerOrDriverId from invoiceId invoiceNumber invoiceType issuedToType issuedToTypes limit offset status to

getFinanceManagementFinanceReconciliation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Lib.Finance.Reconciliation.Types.Domain -> Lib.Finance.Reconciliation.Types.DataSource -> Lib.Finance.Reconciliation.Types.DataSource -> Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.ReconciliationRes)
getFinanceManagementFinanceReconciliation merchantShortId opCity apiTokenInfo fromDate limit offset toDate domain source target = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.getFinanceManagementFinanceReconciliation) fromDate limit offset toDate domain source target

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

getFinanceManagementFinanceInvoicePdf :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Domain.Types.Invoice.InvoiceType -> Kernel.Prelude.Maybe Domain.Types.Invoice.IssuedToType -> Kernel.Prelude.Maybe [Domain.Types.Invoice.IssuedToType] -> Kernel.Prelude.Maybe Kernel.External.Types.Language -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Invoice.InvoiceStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.FinanceInvoicePdfResp)
getFinanceManagementFinanceInvoicePdf merchantShortId opCity apiTokenInfo fleetOwnerOrDriverId from invoiceId invoiceNumber invoiceType issuedToType issuedToTypes language limit offset rideId status to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.getFinanceManagementFinanceInvoicePdf) fleetOwnerOrDriverId from invoiceId invoiceNumber invoiceType issuedToType issuedToTypes language limit offset rideId status to

getFinanceManagementFinanceAuditList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.AuditEntry.AuditEntityType -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.AuditEntry.AuditAction -> Kernel.Prelude.Maybe Lib.Finance.Core.Types.ActorType -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.AuditListRes)
getFinanceManagementFinanceAuditList merchantShortId opCity apiTokenInfo limit offset from to entityType action actorType actorId entityId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.getFinanceManagementFinanceAuditList) limit offset from to entityType action actorType actorId entityId

getFinanceManagementFinanceSapJournals :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.SapJournalEntry.JournalEntryStatus -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.SapJournalEntry.TransactionType -> Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.SapJournalListRes)
getFinanceManagementFinanceSapJournals merchantShortId opCity apiTokenInfo batchId belnr dateFrom dateTo description glNumber limit offset status transactionType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.getFinanceManagementFinanceSapJournals) batchId belnr dateFrom dateTo description glNumber limit offset status transactionType

getFinanceManagementFinanceSapJournalsTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Text -> Lib.Finance.Domain.Types.SapJournalEntry.TransactionType -> Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.SapJournalTransactionsRes)
getFinanceManagementFinanceSapJournalsTransactions merchantShortId opCity apiTokenInfo description limit offset referenceId batchId transactionType = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.getFinanceManagementFinanceSapJournalsTransactions) description limit offset referenceId batchId transactionType

postFinanceManagementTdsReimbursementRequestSubmit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.FinanceManagement.TdsReimbursementRequestSubmitReq -> Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.TdsReimbursementRequestSubmitRes)
postFinanceManagementTdsReimbursementRequestSubmit merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  let requestorId = apiTokenInfo.personId.getId
  SharedLogic.Transaction.withTransactionStoring transaction $ (do API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.postFinanceManagementTdsReimbursementRequestSubmit) requestorId req)

getFinanceManagementTdsReimbursementStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.FinanceManagement.TdsReimbursementQuarter -> Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.TdsReimbursementStatusRes)
getFinanceManagementTdsReimbursementStatus merchantShortId opCity apiTokenInfo assessmentYear quarter = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.getFinanceManagementTdsReimbursementStatus) quarter assessmentYear requestorId

getFinanceManagementTdsReimbursementList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.FinanceManagement.TdsReimbursementQuarter -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.FinanceManagement.TdsReimbursementStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.TdsReimbursementListRes)
getFinanceManagementTdsReimbursementList merchantShortId opCity apiTokenInfo assessmentYear fleetOwnerId from limit offset quarter status tanNumber to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.getFinanceManagementTdsReimbursementList) assessmentYear fleetOwnerId from limit offset quarter status tanNumber to

getFinanceManagementTdsReimbursement :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.FinanceTdsReimbursementRequest -> Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.TdsReimbursementDetailRes)
getFinanceManagementTdsReimbursement merchantShortId opCity apiTokenInfo requestId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.getFinanceManagementTdsReimbursement) requestId

postFinanceManagementFinanceAdjustmentSubmit :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.FinanceManagement.SubmitLedgerAdjustmentReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFinanceManagementFinanceAdjustmentSubmit merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
      requestorName = apiTokenInfo.person.firstName <> " " <> apiTokenInfo.person.lastName
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ do
    API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.postFinanceManagementFinanceAdjustmentSubmit) requestorId requestorName req

getFinanceManagementFinanceAdjustmentList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.LedgerAdjustmentRequest) -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.FinanceManagement.AdjustmentRequestStatus -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Person) -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.FinanceManagement.AdjustmentCategory -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.FinanceManagement.AdjustmentDirection -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Person) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Person) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.Flow API.Types.ProviderPlatform.Management.FinanceManagement.LedgerAdjustmentListRes)
getFinanceManagementFinanceAdjustmentList merchantShortId opCity apiTokenInfo limit offset adjustmentRequestId status personId excludeCurrentAdminMaker category direction referenceType referenceId adminMakerId adminCheckerId from to = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.getFinanceManagementFinanceAdjustmentList) limit offset adjustmentRequestId status personId excludeCurrentAdminMaker category direction referenceType referenceId adminMakerId adminCheckerId from to requestorId

postFinanceManagementFinanceAdjustmentApprove :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.LedgerAdjustmentRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFinanceManagementFinanceAdjustmentApprove merchantShortId opCity apiTokenInfo adjustmentRequestId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
      requestorName = apiTokenInfo.person.firstName <> " " <> apiTokenInfo.person.lastName
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ do
    API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.postFinanceManagementFinanceAdjustmentApprove) adjustmentRequestId requestorId requestorName

postFinanceManagementFinanceAdjustmentReject :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.LedgerAdjustmentRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFinanceManagementFinanceAdjustmentReject merchantShortId opCity apiTokenInfo adjustmentRequestId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
      requestorName = apiTokenInfo.person.firstName <> " " <> apiTokenInfo.person.lastName
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.castEndpoint apiTokenInfo.userActionType) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $ do
    API.Client.ProviderPlatform.Management.callManagementAPI checkedMerchantId opCity (.financeManagementDSL.postFinanceManagementFinanceAdjustmentReject) adjustmentRequestId requestorId requestorName
