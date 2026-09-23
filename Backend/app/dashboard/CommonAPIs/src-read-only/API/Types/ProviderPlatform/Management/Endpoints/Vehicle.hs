{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.Vehicle where

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
import Servant
import Servant.Client

data ParkingFeeExemptionReq = ParkingFeeExemptionReq {exempt :: Kernel.Prelude.Bool, specialLocationId :: Kernel.Prelude.Maybe Kernel.Prelude.Text, reason :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets ParkingFeeExemptionReq where
  hideSecrets = Kernel.Prelude.identity

data VehicleListItem = VehicleListItem
  { rcId :: Kernel.Prelude.Text,
    vehicleNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleMake :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleModel :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleColor :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleClass :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleVariant :: Kernel.Prelude.Maybe Dashboard.Common.VehicleVariant,
    verified :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    approved :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    createdAt :: Kernel.Prelude.UTCTime,
    recentFleetInfo :: Kernel.Prelude.Maybe Dashboard.Common.Driver.DriverAssociationInfo,
    linkedDriverInfo :: Kernel.Prelude.Maybe Dashboard.Common.Driver.DriverAssociationInfo,
    exemptParkingFee :: Kernel.Prelude.Maybe Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data VehicleListRes = VehicleListRes {totalItems :: Kernel.Prelude.Int, summary :: Dashboard.Common.Summary, vehicles :: [VehicleListItem]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("vehicle" :> (GetVehicleList :<|> PostVehicleParkingFeeExemption))

type GetVehicleList =
  ( "list" :> QueryParam "limit" Kernel.Prelude.Int :> QueryParam "offset" Kernel.Prelude.Int :> QueryParam "fleetOwnerId" Kernel.Prelude.Text
      :> QueryParam
           "vehicleNumber"
           Kernel.Prelude.Text
      :> QueryParam "verified" Kernel.Prelude.Bool
      :> QueryParam
           "approvalStatus"
           Dashboard.Common.Driver.ApprovalStatusFilter
      :> QueryParam
           "from"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "to"
           Kernel.Prelude.UTCTime
      :> QueryParam
           "requestorId"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           VehicleListRes
  )

type PostVehicleParkingFeeExemption =
  ( Capture "vehicleNumber" Kernel.Prelude.Text :> "parkingFeeExemption" :> ReqBody '[JSON] ParkingFeeExemptionReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

data VehicleAPIs = VehicleAPIs
  { getVehicleList :: Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Dashboard.Common.Driver.ApprovalStatusFilter -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient VehicleListRes,
    postVehicleParkingFeeExemption :: Kernel.Prelude.Text -> ParkingFeeExemptionReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkVehicleAPIs :: (Client EulerHS.Types.EulerClient API -> VehicleAPIs)
mkVehicleAPIs vehicleClient = (VehicleAPIs {..})
  where
    getVehicleList :<|> postVehicleParkingFeeExemption = vehicleClient

data VehicleUserActionType
  = GET_VEHICLE_LIST
  | POST_VEHICLE_PARKING_FEE_EXEMPTION
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''VehicleUserActionType])
