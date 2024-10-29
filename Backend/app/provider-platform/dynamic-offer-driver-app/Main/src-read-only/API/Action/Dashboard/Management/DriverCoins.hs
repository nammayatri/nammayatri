{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.DriverCoins
  ( API.Types.ProviderPlatform.Management.DriverCoins.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.DriverCoins
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.DriverCoins as Domain.Action.Dashboard.Management.DriverCoins
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.DriverCoins.API)
handler merchantId city = postDriverCoinsBulkUploadCoins merchantId city :<|> postDriverCoinsBulkUploadCoinsV2 merchantId city :<|> getDriverCoinsCoinHistory merchantId city

postDriverCoinsBulkUploadCoins :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.DriverCoins.BulkUploadCoinsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverCoinsBulkUploadCoins a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverCoins.postDriverCoinsBulkUploadCoins a3 a2 a1

postDriverCoinsBulkUploadCoinsV2 :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.DriverCoins.BulkUploadCoinsReqV2 -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverCoinsBulkUploadCoinsV2 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverCoins.postDriverCoinsBulkUploadCoinsV2 a3 a2 a1

getDriverCoinsCoinHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Kernel.Prelude.Maybe Kernel.Prelude.Integer -> Kernel.Prelude.Maybe Kernel.Prelude.Integer -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverCoins.CoinHistoryRes)
getDriverCoinsCoinHistory a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverCoins.getDriverCoinsCoinHistory a5 a4 a3 a2 a1
