{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.DriverGoHome
  ( API.Types.ProviderPlatform.Management.DriverGoHome.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.DriverGoHome
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Management.DriverGoHome
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.DriverGoHome.API)
handler merchantId city = getDriverGoHomeGetHomeLocation merchantId city :<|> postDriverGoHomeUpdateHomeLocation merchantId city :<|> postDriverGoHomeIncrementGoToCount merchantId city :<|> getDriverGoHomeGetGoHomeInfo merchantId city

getDriverGoHomeGetHomeLocation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverGoHome.GetHomeLocationsRes)
getDriverGoHomeGetHomeLocation a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverGoHome.getDriverGoHomeGetHomeLocation a3 a2 a1

postDriverGoHomeUpdateHomeLocation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverGoHome.UpdateDriverHomeLocationReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverGoHomeUpdateHomeLocation a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverGoHome.postDriverGoHomeUpdateHomeLocation a4 a3 a2 a1

postDriverGoHomeIncrementGoToCount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverGoHomeIncrementGoToCount a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverGoHome.postDriverGoHomeIncrementGoToCount a3 a2 a1

getDriverGoHomeGetGoHomeInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverGoHome.CachedGoHomeRequestInfoRes)
getDriverGoHomeGetGoHomeInfo a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverGoHome.getDriverGoHomeGetGoHomeInfo a3 a2 a1
