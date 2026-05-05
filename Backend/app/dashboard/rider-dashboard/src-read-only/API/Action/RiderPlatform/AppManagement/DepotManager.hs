{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement.DepotManager
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.DepotManager
import qualified Domain.Action.RiderPlatform.AppManagement.DepotManager
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("depotManager" :> (PostDepotManagerUpsertOne :<|> PostDepotManagerUpsertMany :<|> GetDepotManagerList :<|> DeleteDepotManager :<|> GetDepotManagerByMobileNumber :<|> GetDepotManagerByDepotCode :<|> GetDepotManagerByPersonId))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postDepotManagerUpsertOne merchantId city :<|> postDepotManagerUpsertMany merchantId city :<|> getDepotManagerList merchantId city :<|> deleteDepotManager merchantId city :<|> getDepotManagerByMobileNumber merchantId city :<|> getDepotManagerByDepotCode merchantId city :<|> getDepotManagerByPersonId merchantId city

type PostDepotManagerUpsertOne =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.DEPOT_MANAGER) / ('API.Types.Dashboard.AppManagement.DepotManager.POST_DEPOT_MANAGER_UPSERT_ONE))
      :> API.Types.Dashboard.AppManagement.DepotManager.PostDepotManagerUpsertOne
  )

type PostDepotManagerUpsertMany =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.DEPOT_MANAGER) / ('API.Types.Dashboard.AppManagement.DepotManager.POST_DEPOT_MANAGER_UPSERT_MANY))
      :> API.Types.Dashboard.AppManagement.DepotManager.PostDepotManagerUpsertMany
  )

type GetDepotManagerList =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.DEPOT_MANAGER) / ('API.Types.Dashboard.AppManagement.DepotManager.GET_DEPOT_MANAGER_LIST))
      :> API.Types.Dashboard.AppManagement.DepotManager.GetDepotManagerList
  )

type DeleteDepotManager =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.DEPOT_MANAGER) / ('API.Types.Dashboard.AppManagement.DepotManager.DELETE_DEPOT_MANAGER))
      :> API.Types.Dashboard.AppManagement.DepotManager.DeleteDepotManager
  )

type GetDepotManagerByMobileNumber =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.DEPOT_MANAGER) / ('API.Types.Dashboard.AppManagement.DepotManager.GET_DEPOT_MANAGER_BY_MOBILE_NUMBER))
      :> API.Types.Dashboard.AppManagement.DepotManager.GetDepotManagerByMobileNumber
  )

type GetDepotManagerByDepotCode =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.DEPOT_MANAGER) / ('API.Types.Dashboard.AppManagement.DepotManager.GET_DEPOT_MANAGER_BY_DEPOT_CODE))
      :> API.Types.Dashboard.AppManagement.DepotManager.GetDepotManagerByDepotCode
  )

type GetDepotManagerByPersonId =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.DEPOT_MANAGER) / ('API.Types.Dashboard.AppManagement.DepotManager.GET_DEPOT_MANAGER_BY_PERSON_ID))
      :> API.Types.Dashboard.AppManagement.DepotManager.GetDepotManagerByPersonId
  )

postDepotManagerUpsertOne :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.AppManagement.DepotManager.DepotManagerDetail -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DepotManager.UpsertDepotManagerResp)
postDepotManagerUpsertOne merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.DepotManager.postDepotManagerUpsertOne merchantShortId opCity apiTokenInfo req

postDepotManagerUpsertMany :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.AppManagement.DepotManager.DepotManagerDetails -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DepotManager.UpsertDepotManagerResp)
postDepotManagerUpsertMany merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.DepotManager.postDepotManagerUpsertMany merchantShortId opCity apiTokenInfo req

getDepotManagerList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DepotManager.DepotManagerListResp)
getDepotManagerList merchantShortId opCity apiTokenInfo limit offset = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.DepotManager.getDepotManagerList merchantShortId opCity apiTokenInfo limit offset

deleteDepotManager :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.AppManagement.DepotManager.DeleteDepotManagerReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteDepotManager merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.DepotManager.deleteDepotManager merchantShortId opCity apiTokenInfo req

getDepotManagerByMobileNumber :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DepotManager.DepotManagerInfo)
getDepotManagerByMobileNumber merchantShortId opCity apiTokenInfo mobileNumber countryCode = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.DepotManager.getDepotManagerByMobileNumber merchantShortId opCity apiTokenInfo mobileNumber countryCode

getDepotManagerByDepotCode :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DepotManager.DepotManagerListResp)
getDepotManagerByDepotCode merchantShortId opCity apiTokenInfo depotCode limit offset = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.DepotManager.getDepotManagerByDepotCode merchantShortId opCity apiTokenInfo depotCode limit offset

getDepotManagerByPersonId :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DepotManager.DepotManagerInfo)
getDepotManagerByPersonId merchantShortId opCity apiTokenInfo personId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.DepotManager.getDepotManagerByPersonId merchantShortId opCity apiTokenInfo personId
