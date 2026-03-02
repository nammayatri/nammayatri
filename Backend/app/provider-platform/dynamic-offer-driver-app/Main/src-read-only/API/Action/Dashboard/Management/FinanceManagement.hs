{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.FinanceManagement
  ( API.Types.ProviderPlatform.Management.FinanceManagement.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.FinanceManagement
import qualified Domain.Action.Dashboard.Management.FinanceManagement
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.FinanceManagement.API)
handler merchantId city = getFinanceManagementSubscriptionPurchaseList merchantId city :<|> getFinanceManagementFinanceInvoiceList merchantId city :<|> getFinanceManagementFinanceReconciliation merchantId city :<|> getFinanceManagementFinancePayoutList merchantId city :<|> getFinanceManagementFinanceWalletLedger merchantId city :<|> getFinanceManagementFinanceEarningSummary merchantId city :<|> postFinanceManagementReconciliationTrigger merchantId city

getFinanceManagementSubscriptionPurchaseList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.SubscriptionPurchaseListRes)
getFinanceManagementSubscriptionPurchaseList a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementSubscriptionPurchaseList a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1

getFinanceManagementFinanceInvoiceList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.InvoiceListRes)
getFinanceManagementFinanceInvoiceList a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementFinanceInvoiceList a9 a8 a7 a6 a5 a4 a3 a2 a1

getFinanceManagementFinanceReconciliation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.ReconciliationRes)
getFinanceManagementFinanceReconciliation a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementFinanceReconciliation a7 a6 a5 a4 a3 a2 a1

getFinanceManagementFinancePayoutList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.PayoutListRes)
getFinanceManagementFinancePayoutList a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementFinancePayoutList a8 a7 a6 a5 a4 a3 a2 a1

getFinanceManagementFinanceWalletLedger :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.WalletLedgerRes)
getFinanceManagementFinanceWalletLedger a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementFinanceWalletLedger a9 a8 a7 a6 a5 a4 a3 a2 a1

getFinanceManagementFinanceEarningSummary :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.EarningsSummaryRes)
getFinanceManagementFinanceEarningSummary a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.getFinanceManagementFinanceEarningSummary a6 a5 a4 a3 a2 a1

postFinanceManagementReconciliationTrigger :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.FinanceManagement.ReconciliationTriggerReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.FinanceManagement.ReconciliationTriggerRes)
postFinanceManagementReconciliationTrigger a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.FinanceManagement.postFinanceManagementReconciliationTrigger a3 a2 a1
