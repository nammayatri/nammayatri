{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.CoinsConfig
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.CoinsConfig
import qualified Domain.Action.Dashboard.Management.CoinsConfig
import qualified Domain.Types.Merchant
import qualified Domain.Types.VehicleCategory
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

type API = ("coinsConfig" :> (GetCoinsConfigList :<|> PutCoinsConfigUpdate :<|> PostCoinsConfigCreate))

type GetCoinsConfigList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/COINS_CONFIG/GET_COINS_CONFIG_LIST"
      :> API.Types.ProviderPlatform.Management.CoinsConfig.GetCoinsConfigList
  )

type PutCoinsConfigUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/COINS_CONFIG/PUT_COINS_CONFIG_UPDATE"
      :> API.Types.ProviderPlatform.Management.CoinsConfig.PutCoinsConfigUpdate
  )

type PostCoinsConfigCreate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/COINS_CONFIG/POST_COINS_CONFIG_CREATE"
      :> API.Types.ProviderPlatform.Management.CoinsConfig.PostCoinsConfigCreate
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getCoinsConfigList merchantId city :<|> putCoinsConfigUpdate merchantId city :<|> postCoinsConfigCreate merchantId city

getCoinsConfigList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.VehicleCategory.VehicleCategory) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.CoinsConfig.CoinsConfigListRes)
getCoinsConfigList a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.CoinsConfig.getCoinsConfigList a7 a6 a4 a3 a2 a1

putCoinsConfigUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.CoinsConfig.UpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putCoinsConfigUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.CoinsConfig.putCoinsConfigUpdate a4 a3 a1

postCoinsConfigCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.CoinsConfig.CreateCoinsConfigReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCoinsConfigCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.CoinsConfig.postCoinsConfigCreate a4 a3 a1
