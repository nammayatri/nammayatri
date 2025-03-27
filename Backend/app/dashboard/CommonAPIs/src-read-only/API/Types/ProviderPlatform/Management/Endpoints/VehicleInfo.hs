{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.VehicleInfo where

import qualified Dashboard.Common
import qualified Dashboard.Common.Driver
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Text
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

newtype UpdateVehicleInfoReq = UpdateVehicleInfoReq {newInfo :: [VehicleInfoAPIEntity]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateVehicleInfoReq where
  hideSecrets = Kernel.Prelude.identity

data VehicleInfoAPIEntity = VehicleInfoAPIEntity
  { id :: Kernel.Types.Id.Id Dashboard.Common.VehicleInfo,
    rcId :: Kernel.Types.Id.Id Dashboard.Common.Driver.VehicleRegistrationCertificate,
    questionName :: Data.Text.Text,
    question :: Data.Text.Text,
    answer :: Data.Text.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("vehicleInfo" :> (GetVehicleInfoList :<|> PutVehicleInfoUpdate))

type GetVehicleInfoList = ("list" :> Get '[JSON] [VehicleInfoAPIEntity])

type PutVehicleInfoUpdate = ("update" :> ReqBody '[JSON] UpdateVehicleInfoReq :> Put '[JSON] Kernel.Types.APISuccess.APISuccess)

data VehicleInfoAPIs = VehicleInfoAPIs
  { getVehicleInfoList :: EulerHS.Types.EulerClient [VehicleInfoAPIEntity],
    putVehicleInfoUpdate :: UpdateVehicleInfoReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkVehicleInfoAPIs :: (Client EulerHS.Types.EulerClient API -> VehicleInfoAPIs)
mkVehicleInfoAPIs vehicleInfoClient = (VehicleInfoAPIs {..})
  where
    getVehicleInfoList :<|> putVehicleInfoUpdate = vehicleInfoClient

data VehicleInfoUserActionType
  = GET_VEHICLE_INFO_LIST
  | PUT_VEHICLE_INFO_UPDATE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''VehicleInfoUserActionType])
