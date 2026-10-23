{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.DepotManager
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.DepotManager
import qualified Domain.Action.Dashboard.AppManagement.DepotManager
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("depotManager" :> (PostDepotManagerUpsertOne :<|> PostDepotManagerUpsertMany :<|> GetDepotManagerList :<|> DeleteDepotManager :<|> GetDepotManagerByMobileNumber :<|> GetDepotManagerByDepotCode))

type PostDepotManagerUpsertOne =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/DEPOT_MANAGER/POST_DEPOT_MANAGER_UPSERT_ONE"
      :> API.Types.Dashboard.AppManagement.DepotManager.PostDepotManagerUpsertOne
  )

type PostDepotManagerUpsertMany =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/DEPOT_MANAGER/POST_DEPOT_MANAGER_UPSERT_MANY"
      :> API.Types.Dashboard.AppManagement.DepotManager.PostDepotManagerUpsertMany
  )

type GetDepotManagerList =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/DEPOT_MANAGER/GET_DEPOT_MANAGER_LIST"
      :> API.Types.Dashboard.AppManagement.DepotManager.GetDepotManagerList
  )

type DeleteDepotManager =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/DEPOT_MANAGER/DELETE_DEPOT_MANAGER"
      :> API.Types.Dashboard.AppManagement.DepotManager.DeleteDepotManager
  )

type GetDepotManagerByMobileNumber =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/DEPOT_MANAGER/GET_DEPOT_MANAGER_BY_MOBILE_NUMBER"
      :> API.Types.Dashboard.AppManagement.DepotManager.GetDepotManagerByMobileNumber
  )

type GetDepotManagerByDepotCode =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/DEPOT_MANAGER/GET_DEPOT_MANAGER_BY_DEPOT_CODE"
      :> API.Types.Dashboard.AppManagement.DepotManager.GetDepotManagerByDepotCode
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postDepotManagerUpsertOne merchantId city :<|> postDepotManagerUpsertMany merchantId city :<|> getDepotManagerList merchantId city :<|> deleteDepotManager merchantId city :<|> getDepotManagerByMobileNumber merchantId city :<|> getDepotManagerByDepotCode merchantId city

postDepotManagerUpsertOne :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.AppManagement.DepotManager.DepotManagerDetail -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DepotManager.UpsertDepotManagerResp)
postDepotManagerUpsertOne a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_APP_MANAGEMENT/DEPOT_MANAGER/POST_DEPOT_MANAGER_UPSERT_ONE" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.DepotManager.postDepotManagerUpsertOne a4 a3 a1
    )

postDepotManagerUpsertMany :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.AppManagement.DepotManager.DepotManagerDetails -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DepotManager.UpsertDepotManagerResp)
postDepotManagerUpsertMany a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_APP_MANAGEMENT/DEPOT_MANAGER/POST_DEPOT_MANAGER_UPSERT_MANY" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.DepotManager.postDepotManagerUpsertMany a4 a3 a1
    )

getDepotManagerList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DepotManager.DepotManagerListResp)
getDepotManagerList a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.DepotManager.getDepotManagerList a5 a4 a2 a1

deleteDepotManager :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.AppManagement.DepotManager.DeleteDepotManagerReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteDepotManager a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_APP_MANAGEMENT/DEPOT_MANAGER/DELETE_DEPOT_MANAGER" a2 (Kernel.Prelude.Just a1)
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.DepotManager.deleteDepotManager a4 a3 a1
    )

getDepotManagerByMobileNumber :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DepotManager.DepotManagerInfo)
getDepotManagerByMobileNumber a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.DepotManager.getDepotManagerByMobileNumber a5 a4 a2 a1

getDepotManagerByDepotCode :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DepotManager.DepotManagerListResp)
getDepotManagerByDepotCode a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.DepotManager.getDepotManagerByDepotCode a6 a5 a3 a2 a1
