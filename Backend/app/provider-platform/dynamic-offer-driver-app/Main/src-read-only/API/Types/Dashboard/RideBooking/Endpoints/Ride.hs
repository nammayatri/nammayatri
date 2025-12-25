{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.RideBooking.Endpoints.Ride where

import qualified Dashboard.Common
import qualified Dashboard.Common.Booking
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

data BookingWithVehicleAndPhoneReq = BookingWithVehicleAndPhoneReq
  { vehicleNumber :: Kernel.Prelude.Text,
    phoneNumber :: Kernel.Prelude.Text,
    countryCode :: Kernel.Prelude.Text,
    endRideForDriver :: Kernel.Prelude.Bool,
    endRideForVehicle :: Kernel.Prelude.Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets BookingWithVehicleAndPhoneReq where
  hideSecrets = Kernel.Prelude.identity

newtype BookingWithVehicleAndPhoneRes = BookingWithVehicleAndPhoneRes {driverId :: Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets BookingWithVehicleAndPhoneRes where
  hideSecrets = Kernel.Prelude.identity

data CancelRideReq = CancelRideReq {reasonCode :: Dashboard.Common.Booking.CancellationReasonCode, additionalInfo :: Kernel.Prelude.Maybe Kernel.Prelude.Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets CancelRideReq where
  hideSecrets = Kernel.Prelude.identity

data EndRideReq = EndRideReq {point :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong, odometerReadingValue :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets EndRideReq where
  hideSecrets = Kernel.Prelude.identity

data StartRideReq = StartRideReq {point :: Kernel.Prelude.Maybe Kernel.External.Maps.Types.LatLong, odometerReadingValue :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets StartRideReq where
  hideSecrets = Kernel.Prelude.identity

type API = ("ride" :> (PostRideStart :<|> PostRideEnd :<|> GetRideCurrentActiveRide :<|> PostRideCancel :<|> PostRideBookingWithVehicleNumberAndPhone))

type PostRideStart = (Capture "rideId" (Kernel.Types.Id.Id Dashboard.Common.Ride) :> "start" :> ReqBody '[JSON] StartRideReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostRideEnd = (Capture "rideId" (Kernel.Types.Id.Id Dashboard.Common.Ride) :> "end" :> ReqBody '[JSON] EndRideReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type GetRideCurrentActiveRide = (Capture "vehicleNumber" Kernel.Prelude.Text :> "currentActiveRide" :> Get '[JSON] (Kernel.Types.Id.Id Dashboard.Common.Ride))

type PostRideCancel = (Capture "rideId" (Kernel.Types.Id.Id Dashboard.Common.Ride) :> "cancel" :> ReqBody '[JSON] CancelRideReq :> Post '[JSON] Kernel.Types.APISuccess.APISuccess)

type PostRideBookingWithVehicleNumberAndPhone = ("booking" :> "withVehicleNumberAndPhone" :> ReqBody '[JSON] BookingWithVehicleAndPhoneReq :> Post '[JSON] BookingWithVehicleAndPhoneRes)

data RideAPIs = RideAPIs
  { postRideStart :: Kernel.Types.Id.Id Dashboard.Common.Ride -> StartRideReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postRideEnd :: Kernel.Types.Id.Id Dashboard.Common.Ride -> EndRideReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    getRideCurrentActiveRide :: Kernel.Prelude.Text -> EulerHS.Types.EulerClient (Kernel.Types.Id.Id Dashboard.Common.Ride),
    postRideCancel :: Kernel.Types.Id.Id Dashboard.Common.Ride -> CancelRideReq -> EulerHS.Types.EulerClient Kernel.Types.APISuccess.APISuccess,
    postRideBookingWithVehicleNumberAndPhone :: BookingWithVehicleAndPhoneReq -> EulerHS.Types.EulerClient BookingWithVehicleAndPhoneRes
  }

mkRideAPIs :: (Client EulerHS.Types.EulerClient API -> RideAPIs)
mkRideAPIs rideClient = (RideAPIs {..})
  where
    postRideStart :<|> postRideEnd :<|> getRideCurrentActiveRide :<|> postRideCancel :<|> postRideBookingWithVehicleNumberAndPhone = rideClient

data RideUserActionType
  = POST_RIDE_START
  | POST_RIDE_END
  | GET_RIDE_CURRENT_ACTIVE_RIDE
  | POST_RIDE_CANCEL
  | POST_RIDE_BOOKING_WITH_VEHICLE_NUMBER_AND_PHONE
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''RideUserActionType])
