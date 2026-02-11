{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.EntityInfo
  ( API.Types.ProviderPlatform.Management.EntityInfo.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.EntityInfo.API)
handler merchantId city = getEntityInfoList merchantId city :<|> postEntityInfoUpdate merchantId city

getEntityInfoList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.EntityInfo.EntityExtraInformation)
getEntityInfoList a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.EntityInfo.getEntityInfoList a4 a3 a2 a1

postEntityInfoUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.EntityInfo.UpdateEntityInfoReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postEntityInfoUpdate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.EntityInfo.postEntityInfoUpdate a3 a2 a1
