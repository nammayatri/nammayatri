{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.DepotManager
  ( API.Types.Dashboard.AppManagement.DepotManager.API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.DepotManager
import qualified Domain.Action.Dashboard.AppManagement.DepotManager
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.DepotManager.API)
handler merchantId city = postDepotManagerUpsertOne merchantId city :<|> postDepotManagerUpsertMany merchantId city :<|> getDepotManagerList merchantId city :<|> deleteDepotManager merchantId city :<|> getDepotManagerByMobileNumber merchantId city :<|> getDepotManagerByDepotCode merchantId city :<|> getDepotManagerByPersonId merchantId city

postDepotManagerUpsertOne :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.DepotManager.DepotManagerDetail -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DepotManager.UpsertDepotManagerResp)
postDepotManagerUpsertOne a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DepotManager.postDepotManagerUpsertOne a3 a2 a1

postDepotManagerUpsertMany :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.DepotManager.DepotManagerDetails -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DepotManager.UpsertDepotManagerResp)
postDepotManagerUpsertMany a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DepotManager.postDepotManagerUpsertMany a3 a2 a1

getDepotManagerList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DepotManager.DepotManagerListResp)
getDepotManagerList a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DepotManager.getDepotManagerList a4 a3 a2 a1

deleteDepotManager :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.DepotManager.DeleteDepotManagerReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteDepotManager a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DepotManager.deleteDepotManager a3 a2 a1

getDepotManagerByMobileNumber :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DepotManager.DepotManagerInfo)
getDepotManagerByMobileNumber a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DepotManager.getDepotManagerByMobileNumber a4 a3 a2 a1

getDepotManagerByDepotCode :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DepotManager.DepotManagerListResp)
getDepotManagerByDepotCode a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DepotManager.getDepotManagerByDepotCode a5 a4 a3 a2 a1

getDepotManagerByPersonId :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DepotManager.DepotManagerInfo)
getDepotManagerByPersonId a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DepotManager.getDepotManagerByPersonId a3 a2 a1
