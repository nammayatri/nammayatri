{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Exophone
  ( API.Types.ProviderPlatform.Management.Exophone.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Exophone
import qualified Domain.Action.Dashboard.Management.Exophone
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.Exophone.API)
handler merchantId city = postExophoneCreate merchantId city :<|> getExophoneList merchantId city :<|> getExophone merchantId city :<|> postExophoneUpdate merchantId city :<|> deleteExophoneDelete merchantId city

postExophoneCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Exophone.CreateExophoneReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Exophone.ExophoneRes)
postExophoneCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Exophone.postExophoneCreate a3 a2 a1

getExophoneList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Exophone.ExophoneListRes)
getExophoneList a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Exophone.getExophoneList a3 a2 a1

getExophone :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Exophone.ExophoneRes)
getExophone a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Exophone.getExophone a3 a2 a1

postExophoneUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.Exophone.UpdateExophoneReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postExophoneUpdate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Exophone.postExophoneUpdate a4 a3 a2 a1

deleteExophoneDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteExophoneDelete a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Exophone.deleteExophoneDelete a3 a2 a1
