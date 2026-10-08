{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.RSFReconciliation
  ( API.Types.ProviderPlatform.Management.RSFReconciliation.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.RSFReconciliation
import qualified Data.Time
import qualified Domain.Action.Dashboard.Management.RSFReconciliation
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.RSFReconciliation.API)
handler merchantId city = getRSFReconciliationRsfOrders merchantId city :<|> getRSFReconciliationRsfUtrs merchantId city :<|> getRSFReconciliationRsfUtr merchantId city :<|> postRSFReconciliationRsfUtrBankVerify merchantId city :<|> postRSFReconciliationRsfAutoAllocation merchantId city :<|> postRSFReconciliationRsfOrdersConfirm merchantId city :<|> postRSFReconciliationRsfSend merchantId city :<|> getRSFReconciliationRsfReconUnmatched merchantId city

getRSFReconciliationRsfOrders :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Management.RSFReconciliation.OrderReconVerdict) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RSFReconciliation.OrderListRes)
getRSFReconciliationRsfOrders a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RSFReconciliation.getRSFReconciliationRsfOrders a8 a7 a6 a5 a4 a3 a2 a1

getRSFReconciliationRsfUtrs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Data.Time.Day) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RSFReconciliation.UtrListRes)
getRSFReconciliationRsfUtrs a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RSFReconciliation.getRSFReconciliationRsfUtrs a7 a6 a5 a4 a3 a2 a1

getRSFReconciliationRsfUtr :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RSFReconciliation.UtrDetailRes)
getRSFReconciliationRsfUtr a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RSFReconciliation.getRSFReconciliationRsfUtr a3 a2 a1

postRSFReconciliationRsfUtrBankVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.RSFReconciliation.BankVerifyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRSFReconciliationRsfUtrBankVerify a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RSFReconciliation.postRSFReconciliationRsfUtrBankVerify a4 a3 a2 a1

postRSFReconciliationRsfAutoAllocation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Data.Time.Day) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RSFReconciliation.AutoAllocationRes)
postRSFReconciliationRsfAutoAllocation a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RSFReconciliation.postRSFReconciliationRsfAutoAllocation a3 a2 a1

postRSFReconciliationRsfOrdersConfirm :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.RSFReconciliation.ManualConfirmReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRSFReconciliationRsfOrdersConfirm a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RSFReconciliation.postRSFReconciliationRsfOrdersConfirm a4 a3 a2 a1

postRSFReconciliationRsfSend :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Data.Time.Day) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RSFReconciliation.SendForDateRes)
postRSFReconciliationRsfSend a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RSFReconciliation.postRSFReconciliationRsfSend a3 a2 a1

getRSFReconciliationRsfReconUnmatched :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.RSFReconciliation.ReconGridListRes)
getRSFReconciliationRsfReconUnmatched a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.RSFReconciliation.getRSFReconciliationRsfReconUnmatched a6 a5 a4 a3 a2 a1
