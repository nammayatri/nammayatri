{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.System
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.System
import qualified Domain.Action.Dashboard.Management.System
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("system" :> PostSystemRunQuery)

type PostSystemRunQuery = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/SYSTEM/POST_SYSTEM_RUN_QUERY" :> API.Types.ProviderPlatform.Management.System.PostSystemRunQuery)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postSystemRunQuery merchantId city

postSystemRunQuery :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.System.QueryData -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSystemRunQuery a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.System.postSystemRunQuery a4 a3 a1
