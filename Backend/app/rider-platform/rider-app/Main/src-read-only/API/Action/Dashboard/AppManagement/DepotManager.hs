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
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.DepotManager.API)
handler merchantId city = postDepotManagerUpsertOne merchantId city :<|> postDepotManagerUpsertMany merchantId city

postDepotManagerUpsertOne :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.DepotManager.DepotManagerDetail -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DepotManager.UpsertDepotManagerResp)
postDepotManagerUpsertOne a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DepotManager.postDepotManagerUpsertOne a3 a2 a1

postDepotManagerUpsertMany :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.DepotManager.DepotManagerDetails -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DepotManager.UpsertDepotManagerResp)
postDepotManagerUpsertMany a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.DepotManager.postDepotManagerUpsertMany a3 a2 a1
