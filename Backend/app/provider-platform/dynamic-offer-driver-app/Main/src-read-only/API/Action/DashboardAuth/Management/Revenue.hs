{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Revenue
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Revenue
import qualified Domain.Action.Dashboard.Management.Revenue
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("revenue" :> (GetRevenueCollectionHistory :<|> GetRevenueAllFeeHistory))

type GetRevenueCollectionHistory =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/REVENUE/GET_REVENUE_COLLECTION_HISTORY"
      :> API.Types.ProviderPlatform.Management.Revenue.GetRevenueCollectionHistory
  )

type GetRevenueAllFeeHistory =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/REVENUE/GET_REVENUE_ALL_FEE_HISTORY"
      :> API.Types.ProviderPlatform.Management.Revenue.GetRevenueAllFeeHistory
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getRevenueCollectionHistory merchantId city :<|> getRevenueAllFeeHistory merchantId city

getRevenueCollectionHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Revenue.CollectionList)
getRevenueCollectionHistory a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Revenue.getRevenueCollectionHistory a7 a6 a4 a3 a2 a1

getRevenueAllFeeHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.UTCTime) -> Environment.FlowHandler [API.Types.ProviderPlatform.Management.Revenue.AllFees])
getRevenueAllFeeHistory a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Revenue.getRevenueAllFeeHistory a5 a4 a2 a1
