{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.FinanceManagement
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.FinanceManagement
import qualified Domain.Action.ProviderPlatform.Management.FinanceManagement
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("financeManagement" :> (GetFinanceManagementSubscriptionPurchaseList :<|> GetFinanceManagementFinanceInvoiceList :<|> GetFinanceManagementFinanceReconciliation :<|> GetFinanceManagementFinancePayoutList :<|> GetFinanceManagementFinanceWalletLedger :<|> GetFinanceManagementFinanceEarningSummary :<|> PostFinanceManagementReconciliationTrigger))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getFinanceManagementSubscriptionPurchaseList merchantId city :<|> getFinanceManagementFinanceInvoiceList merchantId city :<|> getFinanceManagementFinanceReconciliation merchantId city :<|> getFinanceManagementFinancePayoutList merchantId city :<|> getFinanceManagementFinanceWalletLedger merchantId city :<|> getFinanceManagementFinanceEarningSummary merchantId city :<|> postFinanceManagementReconciliationTrigger merchantId city

type GetFinanceManagementSubscriptionPurchaseList =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FINANCE_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FinanceManagement.GET_FINANCE_MANAGEMENT_SUBSCRIPTION_PURCHASE_LIST))
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementSubscriptionPurchaseList
  )

type GetFinanceManagementFinanceInvoiceList =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FINANCE_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FinanceManagement.GET_FINANCE_MANAGEMENT_FINANCE_INVOICE_LIST))
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinanceInvoiceList
  )

type GetFinanceManagementFinanceReconciliation =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FINANCE_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FinanceManagement.GET_FINANCE_MANAGEMENT_FINANCE_RECONCILIATION))
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinanceReconciliation
  )

type GetFinanceManagementFinancePayoutList =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FINANCE_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FinanceManagement.GET_FINANCE_MANAGEMENT_FINANCE_PAYOUT_LIST))
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinancePayoutList
  )

type GetFinanceManagementFinanceWalletLedger =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FINANCE_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FinanceManagement.GET_FINANCE_MANAGEMENT_FINANCE_WALLET_LEDGER))
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinanceWalletLedger
  )

type GetFinanceManagementFinanceEarningSummary =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FINANCE_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FinanceManagement.GET_FINANCE_MANAGEMENT_FINANCE_EARNING_SUMMARY))
      :> API.Types.ProviderPlatform.Management.FinanceManagement.GetFinanceManagementFinanceEarningSummary
  )

type PostFinanceManagementReconciliationTrigger =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FINANCE_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.FinanceManagement.POST_FINANCE_MANAGEMENT_RECONCILIATION_TRIGGER))
      :> API.Types.ProviderPlatform.Management.FinanceManagement.PostFinanceManagementReconciliationTrigger
  )

getFinanceManagementSubscriptionPurchaseList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.SubscriptionPurchaseListRes)
getFinanceManagementSubscriptionPurchaseList merchantShortId opCity apiTokenInfo amountMax amountMin driverId fleetOperatorId from limit offset serviceName status subscriptionId to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementSubscriptionPurchaseList merchantShortId opCity apiTokenInfo amountMax amountMin driverId fleetOperatorId from limit offset serviceName status subscriptionId to

getFinanceManagementFinanceInvoiceList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.InvoiceListRes)
getFinanceManagementFinanceInvoiceList merchantShortId opCity apiTokenInfo from invoiceId invoiceType limit offset status to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementFinanceInvoiceList merchantShortId opCity apiTokenInfo from invoiceId invoiceType limit offset status to

getFinanceManagementFinanceReconciliation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.ReconciliationRes)
getFinanceManagementFinanceReconciliation merchantShortId opCity apiTokenInfo fromDate limit offset reconciliationType toDate = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementFinanceReconciliation merchantShortId opCity apiTokenInfo fromDate limit offset reconciliationType toDate

getFinanceManagementFinancePayoutList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.PayoutListRes)
getFinanceManagementFinancePayoutList merchantShortId opCity apiTokenInfo driverId fleetOperatorId from limit offset to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementFinancePayoutList merchantShortId opCity apiTokenInfo driverId fleetOperatorId from limit offset to

getFinanceManagementFinanceWalletLedger :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.WalletLedgerRes)
getFinanceManagementFinanceWalletLedger merchantShortId opCity apiTokenInfo limit offset driverId fleetOperatorId from to sourceType = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementFinanceWalletLedger merchantShortId opCity apiTokenInfo limit offset driverId fleetOperatorId from to sourceType

getFinanceManagementFinanceEarningSummary :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.EarningsSummaryRes)
getFinanceManagementFinanceEarningSummary merchantShortId opCity apiTokenInfo driverId fleetOwnerId from to = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.getFinanceManagementFinanceEarningSummary merchantShortId opCity apiTokenInfo driverId fleetOwnerId from to

postFinanceManagementReconciliationTrigger :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.FinanceManagement.ReconciliationTriggerReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.ReconciliationTriggerRes)
postFinanceManagementReconciliationTrigger merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.FinanceManagement.postFinanceManagementReconciliationTrigger merchantShortId opCity apiTokenInfo req
