{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Endpoints.DeviceVehicleMapping where

import qualified Data.ByteString.Lazy
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Prelude
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.ServantMultipart
import Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import Servant
import Servant.Client

data DeviceVehicleMappingItem = DeviceVehicleMappingItem {createdAt :: Kernel.Prelude.UTCTime, deviceId :: Kernel.Prelude.Text, gtfsId :: Kernel.Prelude.Text, updatedAt :: Kernel.Prelude.UTCTime, vehicleNo :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DeviceVehicleMappingListRes = DeviceVehicleMappingListRes {mappings :: [DeviceVehicleMappingItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype UpsertDeviceVehicleMappingReq = UpsertDeviceVehicleMappingReq {file :: EulerHS.Prelude.FilePath}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpsertDeviceVehicleMappingReq where
  hideSecrets = Kernel.Prelude.identity

data UpsertDeviceVehicleMappingResp = UpsertDeviceVehicleMappingResp {success :: Kernel.Prelude.Text, totalInserted :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("deviceVehicleMapping" :> (GetDeviceVehicleMappingDeviceVehicleMappingList :<|> PostDeviceVehicleMappingDeviceVehicleMappingUpsert))

type GetDeviceVehicleMappingDeviceVehicleMappingList = ("deviceVehicleMapping" :> "list" :> Get ('[JSON]) DeviceVehicleMappingListRes)

type PostDeviceVehicleMappingDeviceVehicleMappingUpsert =
  ( "deviceVehicleMapping" :> "upsert"
      :> Kernel.ServantMultipart.MultipartForm
           Kernel.ServantMultipart.Tmp
           UpsertDeviceVehicleMappingReq
      :> Post ('[JSON]) UpsertDeviceVehicleMappingResp
  )

data DeviceVehicleMappingAPIs = DeviceVehicleMappingAPIs
  { getDeviceVehicleMappingDeviceVehicleMappingList :: (EulerHS.Types.EulerClient DeviceVehicleMappingListRes),
    postDeviceVehicleMappingDeviceVehicleMappingUpsert :: ((Data.ByteString.Lazy.ByteString, UpsertDeviceVehicleMappingReq) -> EulerHS.Types.EulerClient UpsertDeviceVehicleMappingResp)
  }

mkDeviceVehicleMappingAPIs :: (Client EulerHS.Types.EulerClient API -> DeviceVehicleMappingAPIs)
mkDeviceVehicleMappingAPIs deviceVehicleMappingClient = (DeviceVehicleMappingAPIs {..})
  where
    getDeviceVehicleMappingDeviceVehicleMappingList :<|> postDeviceVehicleMappingDeviceVehicleMappingUpsert = deviceVehicleMappingClient

data DeviceVehicleMappingUserActionType
  = GET_DEVICE_VEHICLE_MAPPING_DEVICE_VEHICLE_MAPPING_LIST
  | POST_DEVICE_VEHICLE_MAPPING_DEVICE_VEHICLE_MAPPING_UPSERT
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''DeviceVehicleMappingUserActionType)])
