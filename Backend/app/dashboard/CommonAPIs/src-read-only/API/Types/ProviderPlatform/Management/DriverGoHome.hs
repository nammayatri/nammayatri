{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.DriverGoHome where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data CachedGoHomeRequestInfoRes = CachedGoHomeRequestInfoRes
  { status :: Kernel.Prelude.Maybe Kernel.Prelude.String,
    cnt :: Kernel.Prelude.Int,
    validTill :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    driverGoHomeRequestId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.DriverGoHomeRequest),
    isOnRide :: Kernel.Prelude.Bool,
    goHomeReferenceTime :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CachedGoHomeRequestInfoRes where
  hideSecrets = Kernel.Prelude.identity

data DriverHomeLocationAPIEntity = DriverHomeLocationAPIEntity
  { id :: Kernel.Types.Id.Id Dashboard.Common.DriverHomeLocation,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    address :: Kernel.Prelude.Text,
    tag :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets DriverHomeLocationAPIEntity where
  hideSecrets = Kernel.Prelude.identity

type GetHomeLocationsRes = [API.Types.ProviderPlatform.Management.DriverGoHome.DriverHomeLocationAPIEntity]

type UpdateDriverHomeLocationReq = API.Types.ProviderPlatform.Management.DriverGoHome.DriverHomeLocationAPIEntity

type API = ("driver" :> (GetDriverGoHomeGetHomeLocation :<|> PostDriverGoHomeUpdateHomeLocation :<|> PostDriverGoHomeIncrementGoToCount :<|> GetDriverGoHomeGetGoHomeInfo))

type GetDriverGoHomeGetHomeLocation =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "getHomeLocation"
      :> Get
           '[JSON]
           API.Types.ProviderPlatform.Management.DriverGoHome.GetHomeLocationsRes
  )

type PostDriverGoHomeUpdateHomeLocation =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "updateHomeLocation"
      :> ReqBody
           '[JSON]
           API.Types.ProviderPlatform.Management.DriverGoHome.UpdateDriverHomeLocationReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
  )

type PostDriverGoHomeIncrementGoToCount = (Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "incrementGoToCount" :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetDriverGoHomeGetGoHomeInfo =
  ( Capture "driverId" (Kernel.Types.Id.Id Dashboard.Common.Driver) :> "getGoHomeInfo"
      :> Get
           '[JSON]
           API.Types.ProviderPlatform.Management.DriverGoHome.CachedGoHomeRequestInfoRes
  )

data DriverGoHomeAPIs = DriverGoHomeAPIs
  { getDriverGoHomeGetHomeLocation :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.DriverGoHome.GetHomeLocationsRes,
    postDriverGoHomeUpdateHomeLocation :: Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.ProviderPlatform.Management.DriverGoHome.UpdateDriverHomeLocationReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postDriverGoHomeIncrementGoToCount :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getDriverGoHomeGetGoHomeInfo :: Kernel.Types.Id.Id Dashboard.Common.Driver -> EulerHS.Types.EulerClient API.Types.ProviderPlatform.Management.DriverGoHome.CachedGoHomeRequestInfoRes
  }

mkDriverGoHomeAPIs :: (Client EulerHS.Types.EulerClient API -> DriverGoHomeAPIs)
mkDriverGoHomeAPIs driverGoHomeClient = (DriverGoHomeAPIs {..})
  where
    getDriverGoHomeGetHomeLocation :<|> postDriverGoHomeUpdateHomeLocation :<|> postDriverGoHomeIncrementGoToCount :<|> getDriverGoHomeGetGoHomeInfo = driverGoHomeClient

data DriverGoHomeUserActionType
  = GET_DRIVER_GO_HOME_GET_HOME_LOCATION
  | POST_DRIVER_GO_HOME_UPDATE_HOME_LOCATION
  | POST_DRIVER_GO_HOME_INCREMENT_GO_TO_COUNT
  | GET_DRIVER_GO_HOME_GET_GO_HOME_INFO
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''DriverGoHomeUserActionType])
