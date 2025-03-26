{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.VehicleInfo where

import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Domain.Types.VehicleInfo
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import Servant
import Servant.Client

newtype UpdateVehicleInfoReq = UpdateVehicleInfoReq {newInfo :: [Domain.Types.VehicleInfo.VehicleInfo]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("vehicleInfo" :> (GetVehicleInfoList :<|> PutVehicleInfoUpdate))

type GetVehicleInfoList = ("list" :> Get ('[JSON]) [Domain.Types.VehicleInfo.VehicleInfo])

type PutVehicleInfoUpdate = ("update" :> ReqBody ('[JSON]) UpdateVehicleInfoReq :> Put ('[JSON]) Kernel.Types.APISuccess.APISuccess)

data VehicleInfoAPIs = VehicleInfoAPIs
  { getVehicleInfoList :: (EulerHS.Types.EulerClient [Domain.Types.VehicleInfo.VehicleInfo]),
    putVehicleInfoUpdate :: (UpdateVehicleInfoReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess)
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

$(Data.Singletons.TH.genSingletons [(''VehicleInfoUserActionType)])
