{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.VehicleInfo where

import qualified Dashboard.Common
import qualified Dashboard.Common.Driver
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

newtype UpdateVehicleInfoReq = UpdateVehicleInfoReq {newInfo :: [VehicleInfoAPIEntity]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateVehicleInfoReq where
  hideSecrets = Kernel.Prelude.identity

data VehicleInfoAPIEntity = VehicleInfoAPIEntity
  { id :: Kernel.Types.Id.Id Dashboard.Common.VehicleInfo,
    rcId :: Kernel.Types.Id.Id Dashboard.Common.Driver.VehicleRegistrationCertificate,
    questionName :: Kernel.Prelude.Text,
    question :: Kernel.Prelude.Text,
    answer :: Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("vehicleInfo" :> (GetVehicleInfoList :<|> PostVehicleInfoUpdate))

type GetVehicleInfoList = (Capture "rcId" (Kernel.Types.Id.Id Dashboard.Common.Driver.VehicleRegistrationCertificate) :> "list" :> Get '[JSON] [VehicleInfoAPIEntity])

type PostVehicleInfoUpdate = ("update" :> ReqBody '[JSON] UpdateVehicleInfoReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

data VehicleInfoAPIs = VehicleInfoAPIs
  { getVehicleInfoList :: Kernel.Types.Id.Id Dashboard.Common.Driver.VehicleRegistrationCertificate -> EulerHS.Types.EulerClient [VehicleInfoAPIEntity],
    postVehicleInfoUpdate :: UpdateVehicleInfoReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkVehicleInfoAPIs :: (Client EulerHS.Types.EulerClient API -> VehicleInfoAPIs)
mkVehicleInfoAPIs vehicleInfoClient = (VehicleInfoAPIs {..})
  where
    getVehicleInfoList :<|> postVehicleInfoUpdate = vehicleInfoClient

data VehicleInfoUserActionType
  = GET_VEHICLE_INFO_LIST
  | POST_VEHICLE_INFO_UPDATE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''VehicleInfoUserActionType])
