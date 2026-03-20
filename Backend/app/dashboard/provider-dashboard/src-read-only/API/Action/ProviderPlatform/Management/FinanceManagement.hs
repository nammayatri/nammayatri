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
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import qualified Lib.Finance.Domain.Types.Invoice
import qualified Lib.Finance.Domain.Types.ReconciliationSummary
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("financeManagement" :> (GetFinanceManagementSubscriptionPurchaseList :<|> GetFinanceManagementFinanceInvoiceList :<|> GetFinanceManagementFinanceReconciliation :<|> GetFinanceManagementFinancePaymentSettlementList :<|> GetFinanceManagementFinancePaymentGatewayTransactionList :<|> GetFinanceManagementFinanceWalletLedger :<|> PostFinanceManagementReconciliationTrigger))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getFinanceManagementSubscriptionPurchaseList merchantId city :<|> getFinanceManagementFinanceInvoiceList merchantId city :<|> getFinanceManagementFinanceReconciliation merchantId city :<|> getFinanceManagementFinancePaymentSettlementList merchantId city :<|> getFinanceManagementFinancePaymentGatewayTransactionList merchantId city :<|> getFinanceManagementFinanceWalletLedger merchantId city :<|> postFinanceManagementReconciliationTrigger merchantId city

type GetFinanceManagementSubscriptionPurchaseList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FINANCE_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FinanceManagement.GET_FINANCE_MANAGEMENT_SUBSCRIPTION_PURCHASE_LIST)
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementSubscriptionPurchaseList
  )

type GetFinanceManagementFinanceInvoiceList =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FINANCE_MANAGEMENT / 'API.Types.ProviderPlatform.Management.FinanceManagement.GET_FINANCE_MANAGEMENT_FINANCE_INVOICE_LIST)
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinanceInvoiceList
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

getFinanceManagementSubscriptionPurchaseList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.FinanceManagement.SubscriptionPurchaseStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.SubscriptionPurchaseListRes)
getFinanceManagementSubscriptionPurchaseList merchantShortId opCity apiTokenInfo amountMax amountMin driverId fleetOwnerId from limit offset serviceName status subscriptionId to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementSubscriptionPurchaseList merchantShortId opCity apiTokenInfo amountMax amountMin driverId fleetOwnerId from limit offset serviceName status subscriptionId to

getFinanceManagementFinanceInvoiceList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Invoice.InvoiceType -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Invoice.InvoiceStatus -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.InvoiceListRes)
getFinanceManagementFinanceInvoiceList merchantShortId opCity apiTokenInfo fleetOwnerOrDriverId from invoiceId invoiceNumber invoiceType limit offset status to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementFinanceInvoiceList merchantShortId opCity apiTokenInfo fleetOwnerOrDriverId from invoiceId invoiceNumber invoiceType limit offset status to

getFinanceManagementFinanceReconciliation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Lib.Finance.Domain.Types.ReconciliationSummary.ReconciliationType -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.ReconciliationRes)
getFinanceManagementFinanceReconciliation merchantShortId opCity apiTokenInfo fromDate limit offset toDate reconciliationType = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementFinanceReconciliation merchantShortId opCity apiTokenInfo fromDate limit offset toDate reconciliationType

getFinanceManagementFinancePaymentSettlementList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.PaymentOrder) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.FinanceManagement.PgGateway -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.PGPaymentSettlementReport) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.SubscriptionPurchase) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.PaymentSettlementListRes)
getFinanceManagementFinancePaymentSettlementList merchantShortId opCity apiTokenInfo from limit offset paymentOrderShortId pgApprovalCode pgGateway settlementAmountMax settlementAmountMin settlementFrom settlementId settlementTo settlementUtr subscriptionPurchaseId to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementFinancePaymentSettlementList merchantShortId opCity apiTokenInfo from limit offset paymentOrderShortId pgApprovalCode pgGateway settlementAmountMax settlementAmountMin settlementFrom settlementId settlementTo settlementUtr subscriptionPurchaseId to

getFinanceManagementFinancePaymentGatewayTransactionList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.FinanceManagement.PaymentModeFilter -> Kernel.Prelude.Maybe (Kernel.Types.Id.ShortId Dashboard.Common.PaymentOrder) -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.FinanceManagement.PaymentStatusFilter -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Management.FinanceManagement.PgGateway -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.SubscriptionPurchase) -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.PaymentTransactionReportListRes)
getFinanceManagementFinancePaymentGatewayTransactionList merchantShortId opCity apiTokenInfo from limit offset paymentMode paymentOrderId paymentStatus pgGateway subscriptionId to txnAmountMax txnAmountMin = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementFinancePaymentGatewayTransactionList merchantShortId opCity apiTokenInfo from limit offset paymentMode paymentOrderId paymentStatus pgGateway subscriptionId to txnAmountMax txnAmountMin

getFinanceManagementFinanceWalletLedger :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.WalletLedgerRes)
getFinanceManagementFinanceWalletLedger merchantShortId opCity apiTokenInfo limit offset driverId fleetOperatorId from to sourceType = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementFinanceWalletLedger merchantShortId opCity apiTokenInfo limit offset driverId fleetOperatorId from to sourceType

postFinanceManagementReconciliationTrigger :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.FinanceManagement.ReconciliationTriggerReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.ReconciliationTriggerRes)
postFinanceManagementReconciliationTrigger merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.postFinanceManagementReconciliationTrigger merchantShortId opCity apiTokenInfo req
