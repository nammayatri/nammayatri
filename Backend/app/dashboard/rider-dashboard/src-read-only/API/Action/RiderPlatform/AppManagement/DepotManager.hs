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
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("depotManager" :> (PostDepotManagerUpsertOne :<|> PostDepotManagerUpsertMany))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postDepotManagerUpsertOne merchantId city :<|> postDepotManagerUpsertMany merchantId city

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

postDepotManagerUpsertOne :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.AppManagement.DepotManager.DepotManagerDetail -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DepotManager.UpsertDepotManagerResp)
postDepotManagerUpsertOne merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.DepotManager.postDepotManagerUpsertOne merchantShortId opCity apiTokenInfo req

postDepotManagerUpsertMany :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.AppManagement.DepotManager.DepotManagerDetails -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DepotManager.UpsertDepotManagerResp)
postDepotManagerUpsertMany merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.DepotManager.postDepotManagerUpsertMany merchantShortId opCity apiTokenInfo req
