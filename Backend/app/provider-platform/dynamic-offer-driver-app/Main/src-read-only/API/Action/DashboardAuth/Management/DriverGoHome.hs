{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.DriverGoHome
  ( API,
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
import Tools.Auth.DashboardUserAuth

type API = ("driver" :> (GetDriverGoHomeGetHomeLocation :<|> PostDriverGoHomeUpdateHomeLocation :<|> PostDriverGoHomeIncrementGoToCount :<|> GetDriverGoHomeGetGoHomeInfo))

type GetDriverGoHomeGetHomeLocation =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_GO_HOME/GET_DRIVER_GO_HOME_GET_HOME_LOCATION"
      :> API.Types.ProviderPlatform.Management.DriverGoHome.GetDriverGoHomeGetHomeLocation
  )

type PostDriverGoHomeUpdateHomeLocation =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_GO_HOME/POST_DRIVER_GO_HOME_UPDATE_HOME_LOCATION"
      :> API.Types.ProviderPlatform.Management.DriverGoHome.PostDriverGoHomeUpdateHomeLocation
  )

type PostDriverGoHomeIncrementGoToCount =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_GO_HOME/POST_DRIVER_GO_HOME_INCREMENT_GO_TO_COUNT"
      :> API.Types.ProviderPlatform.Management.DriverGoHome.PostDriverGoHomeIncrementGoToCount
  )

type GetDriverGoHomeGetGoHomeInfo =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/DRIVER_GO_HOME/GET_DRIVER_GO_HOME_GET_GO_HOME_INFO"
      :> API.Types.ProviderPlatform.Management.DriverGoHome.GetDriverGoHomeGetGoHomeInfo
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getDriverGoHomeGetHomeLocation merchantId city :<|> postDriverGoHomeUpdateHomeLocation merchantId city :<|> postDriverGoHomeIncrementGoToCount merchantId city :<|> getDriverGoHomeGetGoHomeInfo merchantId city

getDriverGoHomeGetHomeLocation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverGoHome.GetHomeLocationsRes)
getDriverGoHomeGetHomeLocation a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverGoHome.getDriverGoHomeGetHomeLocation a4 a3 a1

postDriverGoHomeUpdateHomeLocation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverGoHome.UpdateDriverHomeLocationReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverGoHomeUpdateHomeLocation a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverGoHome.postDriverGoHomeUpdateHomeLocation a5 a4 a2 a1

postDriverGoHomeIncrementGoToCount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverGoHomeIncrementGoToCount a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverGoHome.postDriverGoHomeIncrementGoToCount a4 a3 a1

getDriverGoHomeGetGoHomeInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverGoHome.CachedGoHomeRequestInfoRes)
getDriverGoHomeGetGoHomeInfo a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.DriverGoHome.getDriverGoHomeGetGoHomeInfo a4 a3 a1
