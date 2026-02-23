{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking.Endpoints.Volunteer where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Maps.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Kernel.Types.Id
import Servant
import Servant.Client

data AssignCreateAndStartOtpRideAPIReq = AssignCreateAndStartOtpRideAPIReq {bookingId :: Kernel.Types.Id.Id Dashboard.Common.Booking, driverId :: Kernel.Types.Id.Id Dashboard.Common.Driver, point :: Kernel.External.Maps.Types.LatLong}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets AssignCreateAndStartOtpRideAPIReq where
  hideSecrets = Kernel.Prelude.identity

data BookingInfoResponse = BookingInfoResponse
  { bookingId :: Kernel.Types.Id.Id Dashboard.Common.Booking,
    displayBookingId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fromLocation :: Location,
    toLocation :: Kernel.Prelude.Maybe Location,
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    estimatedDistanceWithUnit :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    estimatedFare :: Kernel.Types.Common.Money,
    estimatedFareWithCurrency :: Kernel.Types.Common.PriceAPIEntity,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    riderName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    vehicleVariant :: Dashboard.Common.VehicleVariant
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Location = Location
  { id :: Kernel.Types.Id.Id Location,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    address :: LocationAddress,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LocationAddress = LocationAddress
  { street :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    city :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    state :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    country :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    building :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    areaCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    area :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    door :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fullAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type API = ("volunteer" :> (GetVolunteerBooking :<|> PostVolunteerAssignStartOtpRide))

type GetVolunteerBooking = (Capture "bookingOtp" Kernel.Prelude.Text :> "booking" :> Get '[JSON] BookingInfoResponse)

type PostVolunteerAssignStartOtpRide = ("assign" :> "start" :> ReqBody '[JSON] AssignCreateAndStartOtpRideAPIReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

data VolunteerAPIs = VolunteerAPIs
  { getVolunteerBooking :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient BookingInfoResponse,
    postVolunteerAssignStartOtpRide :: AssignCreateAndStartOtpRideAPIReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess
  }

mkVolunteerAPIs :: (Client EulerHS.Types.EulerClient API -> VolunteerAPIs)
mkVolunteerAPIs volunteerClient = (VolunteerAPIs {..})
  where
    getVolunteerBooking :<|> postVolunteerAssignStartOtpRide = volunteerClient

data VolunteerUserActionType
  = GET_VOLUNTEER_BOOKING
  | POST_VOLUNTEER_ASSIGN_START_OTP_RIDE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''VolunteerUserActionType])
