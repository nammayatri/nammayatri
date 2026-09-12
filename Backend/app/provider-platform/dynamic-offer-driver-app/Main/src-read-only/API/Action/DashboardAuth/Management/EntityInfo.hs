{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.EntityInfo
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.EntityInfo
import qualified Domain.Action.Dashboard.Management.EntityInfo
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

type API = ("entityInfo" :> (GetEntityInfoList :<|> PostEntityInfoUpdate))

type GetEntityInfoList = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/ENTITY_INFO/GET_ENTITY_INFO_LIST" :> API.Types.ProviderPlatform.Management.EntityInfo.GetEntityInfoList)

type PostEntityInfoUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/ENTITY_INFO/POST_ENTITY_INFO_UPDATE"
      :> API.Types.ProviderPlatform.Management.EntityInfo.PostEntityInfoUpdate
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getEntityInfoList merchantId city :<|> postEntityInfoUpdate merchantId city

getEntityInfoList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.EntityInfo.EntityExtraInformation)
getEntityInfoList a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.EntityInfo.getEntityInfoList a5 a4 a2 a1

postEntityInfoUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.EntityInfo.UpdateEntityInfoReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postEntityInfoUpdate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.EntityInfo.postEntityInfoUpdate a4 a3 a1
