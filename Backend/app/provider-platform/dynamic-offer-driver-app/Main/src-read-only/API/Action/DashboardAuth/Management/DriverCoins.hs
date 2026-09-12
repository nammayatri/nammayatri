{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.DriverCoins
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.DriverCoins
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.DriverCoins
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
import Tools.Auth.DashboardUserAuth

type API = ("coins" :> (PostDriverCoinsBulkUploadCoins :<|> PostDriverCoinsBulkUploadCoinsV2 :<|> GetDriverCoinsCoinHistory :<|> PostDriverCoinsBlacklistedEventsUpdate))

type PostDriverCoinsBulkUploadCoins =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_COINS/POST_DRIVER_COINS_BULK_UPLOAD_COINS"
      :> API.Types.ProviderPlatform.Management.DriverCoins.PostDriverCoinsBulkUploadCoins
  )

type PostDriverCoinsBulkUploadCoinsV2 =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_COINS/POST_DRIVER_COINS_BULK_UPLOAD_COINS_V2"
      :> API.Types.ProviderPlatform.Management.DriverCoins.PostDriverCoinsBulkUploadCoinsV2
  )

type GetDriverCoinsCoinHistory =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_COINS/GET_DRIVER_COINS_COIN_HISTORY"
      :> API.Types.ProviderPlatform.Management.DriverCoins.GetDriverCoinsCoinHistory
  )

type PostDriverCoinsBlacklistedEventsUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_COINS/POST_DRIVER_COINS_BLACKLISTED_EVENTS_UPDATE"
      :> API.Types.ProviderPlatform.Management.DriverCoins.PostDriverCoinsBlacklistedEventsUpdate
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postDriverCoinsBulkUploadCoins merchantId city :<|> postDriverCoinsBulkUploadCoinsV2 merchantId city :<|> getDriverCoinsCoinHistory merchantId city :<|> postDriverCoinsBlacklistedEventsUpdate merchantId city

postDriverCoinsBulkUploadCoins :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.DriverCoins.BulkUploadCoinsReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverCoins.BulkUploadCoinRes)
postDriverCoinsBulkUploadCoins a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverCoins.postDriverCoinsBulkUploadCoins a4 a3 a1

postDriverCoinsBulkUploadCoinsV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.DriverCoins.BulkUploadCoinsReqV2 -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverCoins.BulkUploadCoinRes)
postDriverCoinsBulkUploadCoinsV2 a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverCoins.postDriverCoinsBulkUploadCoinsV2 a4 a3 a1

getDriverCoinsCoinHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe (Kernel.Prelude.Integer) -> Kernel.Prelude.Maybe (Kernel.Prelude.Integer) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverCoins.CoinHistoryRes)
getDriverCoinsCoinHistory a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverCoins.getDriverCoinsCoinHistory a6 a5 a3 a2 a1

postDriverCoinsBlacklistedEventsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverCoins.UpdateBlacklistedCoinEventsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverCoinsBlacklistedEventsUpdate a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverCoins.postDriverCoinsBlacklistedEventsUpdate a5 a4 a2 a1
