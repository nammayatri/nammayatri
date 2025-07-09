{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.VehicleInfo where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Servant
import Servant.Client

data UpdateVehicleInfoReq = UpdateVehicleInfoReq {rcNo :: Kernel.Prelude.Text, newInfo :: [VehicleInfoPostData]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateVehicleInfoReq where
  hideSecrets = Kernel.Prelude.identity

data VehicleExtraInformation = VehicleExtraInformation {rcNo :: Kernel.Prelude.Text, mediaUploaded :: Kernel.Prelude.Bool, vehicleInfo :: [VehicleInfoAPIEntity]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleInfoAPIEntity = VehicleInfoAPIEntity {questionId :: Kernel.Prelude.Text, question :: Kernel.Prelude.Text, answer :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleInfoPostData = VehicleInfoPostData {questionId :: Kernel.Prelude.Text, question :: Kernel.Prelude.Text, answer :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("vehicleInfo" :> (GetVehicleInfoList :<|> PostVehicleInfoUpdate))

type GetVehicleInfoList = (Capture "rcNo" Kernel.Prelude.Text :> "list" :> Get '[JSON] VehicleExtraInformation)

type PostVehicleInfoUpdate = ("update" :> ReqBody '[JSON] UpdateVehicleInfoReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

data VehicleInfoAPIs = VehicleInfoAPIs
  { getVehicleInfoList :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient VehicleExtraInformation,
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
