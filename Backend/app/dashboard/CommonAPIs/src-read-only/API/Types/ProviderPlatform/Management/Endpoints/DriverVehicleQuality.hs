{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.ProviderPlatform.Management.Endpoints.DriverVehicleQuality where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time.Calendar
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data DriverVehicleQualityListRes = DriverVehicleQualityListRes {totalItems :: Kernel.Prelude.Int, drivers :: [DriverVehicleQualityResp]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DriverVehicleQualityResp = DriverVehicleQualityResp
  { driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver,
    driverName :: Kernel.Prelude.Text,
    phoneNumber :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleAge :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    manufacturingDate :: Kernel.Prelude.Maybe Data.Time.Calendar.Day,
    driverRating :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    totalRides :: Kernel.Prelude.Int,
    feedbackBadges :: [FeedbackBadgeCount],
    vehicleVariant :: Dashboard.Common.VehicleVariant,
    vehicleModel :: Kernel.Prelude.Text,
    vehicleMake :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fuelType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleColor :: Kernel.Prelude.Text,
    registrationNo :: Kernel.Prelude.Text,
    airConditioned :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    vehicleName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleCapacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    vehicleRating :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    vehicleRatingRemark :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data FeedbackBadgeCount = FeedbackBadgeCount {badge :: Kernel.Prelude.Text, badgeCount :: Kernel.Prelude.Int}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data UpdateVehicleRatingReq = UpdateVehicleRatingReq {registrationNo :: Kernel.Prelude.Text, rating :: Kernel.Prelude.Double, remark :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets UpdateVehicleRatingReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("driverVehicleQuality" :> (GetDriverVehicleQualityList :<|> GetDriverVehicleQualitySearch :<|> PostDriverVehicleQualityUpdateVehicleRating))

type GetDriverVehicleQualityList =
  ( "list" :> QueryParam "vehicleVariant" Dashboard.Common.VehicleVariant :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "maxVehicleRating"
           Kernel.Prelude.Double
      :> QueryParam "minVehicleRating" Kernel.Prelude.Double
      :> QueryParam
           "vehicleVariants"
           [Dashboard.Common.VehicleVariant]
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> MandatoryQueryParam
           "maxVehicleAge"
           Kernel.Prelude.Int
      :> MandatoryQueryParam
           "minDriverRating"
           Kernel.Prelude.Double
      :> Get
           '[JSON]
           DriverVehicleQualityListRes
  )

type GetDriverVehicleQualitySearch = ("search" :> QueryParam "phoneNumber" Kernel.Prelude.Text :> QueryParam "vehicleNumber" Kernel.Prelude.Text :> Get '[JSON] [DriverVehicleQualityResp])

type PostDriverVehicleQualityUpdateVehicleRating = ("updateVehicleRating" :> ReqBody '[JSON] UpdateVehicleRatingReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

data DriverVehicleQualityAPIs = DriverVehicleQualityAPIs
  { getDriverVehicleQualityList :: Kernel.Prelude.Maybe Dashboard.Common.VehicleVariant -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Prelude.Maybe Kernel.Prelude.Double -> Kernel.Prelude.Maybe [Dashboard.Common.VehicleVariant] -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Int -> Kernel.Prelude.Double -> EulerHS.Types.EulerClient DriverVehicleQualityListRes,
    getDriverVehicleQualitySearch :: Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> EulerHS.Types.EulerClient [DriverVehicleQualityResp],
    postDriverVehicleQualityUpdateVehicleRating :: UpdateVehicleRatingReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkDriverVehicleQualityAPIs :: (Client EulerHS.Types.EulerClient API -> DriverVehicleQualityAPIs)
mkDriverVehicleQualityAPIs driverVehicleQualityClient = (DriverVehicleQualityAPIs {..})
  where
    getDriverVehicleQualityList :<|> getDriverVehicleQualitySearch :<|> postDriverVehicleQualityUpdateVehicleRating = driverVehicleQualityClient

data DriverVehicleQualityUserActionType
  = GET_DRIVER_VEHICLE_QUALITY_LIST
  | GET_DRIVER_VEHICLE_QUALITY_SEARCH
  | POST_DRIVER_VEHICLE_QUALITY_UPDATE_VEHICLE_RATING
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''DriverVehicleQualityUserActionType])
