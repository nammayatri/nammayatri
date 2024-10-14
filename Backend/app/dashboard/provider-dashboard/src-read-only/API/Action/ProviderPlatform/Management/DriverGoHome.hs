{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.DriverGoHome
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.DriverGoHome
import qualified Dashboard.Common
import qualified Domain.Action.ProviderPlatform.Management.DriverGoHome
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("driver" :> (GetDriverGoHomeGetHomeLocation :<|> PostDriverGoHomeUpdateHomeLocation :<|> PostDriverGoHomeIncrementGoToCount :<|> GetDriverGoHomeGetGoHomeInfo))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getDriverGoHomeGetHomeLocation merchantId city :<|> postDriverGoHomeUpdateHomeLocation merchantId city :<|> postDriverGoHomeIncrementGoToCount merchantId city :<|> getDriverGoHomeGetGoHomeInfo merchantId city

type GetDriverGoHomeGetHomeLocation =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_GO_HOME / 'API.Types.ProviderPlatform.Management.DriverGoHome.GET_DRIVER_GO_HOME_GET_HOME_LOCATION)
      :> API.Types.ProviderPlatform.Management.DriverGoHome.GetDriverGoHomeGetHomeLocation
  )

type PostDriverGoHomeUpdateHomeLocation =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_GO_HOME / 'API.Types.ProviderPlatform.Management.DriverGoHome.POST_DRIVER_GO_HOME_UPDATE_HOME_LOCATION)
      :> API.Types.ProviderPlatform.Management.DriverGoHome.PostDriverGoHomeUpdateHomeLocation
  )

type PostDriverGoHomeIncrementGoToCount =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_GO_HOME / 'API.Types.ProviderPlatform.Management.DriverGoHome.POST_DRIVER_GO_HOME_INCREMENT_GO_TO_COUNT)
      :> API.Types.ProviderPlatform.Management.DriverGoHome.PostDriverGoHomeIncrementGoToCount
  )

type GetDriverGoHomeGetGoHomeInfo =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_MANAGEMENT / 'API.Types.ProviderPlatform.Management.DRIVER_GO_HOME / 'API.Types.ProviderPlatform.Management.DriverGoHome.GET_DRIVER_GO_HOME_GET_GO_HOME_INFO)
      :> API.Types.ProviderPlatform.Management.DriverGoHome.GetDriverGoHomeGetGoHomeInfo
  )

getDriverGoHomeGetHomeLocation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverGoHome.GetHomeLocationsRes)
getDriverGoHomeGetHomeLocation merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverGoHome.getDriverGoHomeGetHomeLocation merchantShortId opCity apiTokenInfo driverId

postDriverGoHomeUpdateHomeLocation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverGoHome.UpdateDriverHomeLocationReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverGoHomeUpdateHomeLocation merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverGoHome.postDriverGoHomeUpdateHomeLocation merchantShortId opCity apiTokenInfo driverId req

postDriverGoHomeIncrementGoToCount :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverGoHomeIncrementGoToCount merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverGoHome.postDriverGoHomeIncrementGoToCount merchantShortId opCity apiTokenInfo driverId

getDriverGoHomeGetGoHomeInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Environment.FlowHandler API.Types.ProviderPlatform.Management.DriverGoHome.CachedGoHomeRequestInfoRes)
getDriverGoHomeGetGoHomeInfo merchantShortId opCity apiTokenInfo driverId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverGoHome.getDriverGoHomeGetGoHomeInfo merchantShortId opCity apiTokenInfo driverId
