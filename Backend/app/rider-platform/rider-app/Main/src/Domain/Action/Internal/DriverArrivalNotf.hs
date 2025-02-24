module Domain.Action.Internal.DriverArrivalNotf where

import Data.Aeson
import Data.Text (pack)
import Domain.Types.Ride
import Environment (Flow)
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Error
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Notifications as Notify

data RideNotificationStatus
  = IDLE
  | DRIVER_ON_THE_WAY
  | DRIVER_REACHING
  | DRIVER_REACHED
  deriving (Show, Eq, Generic, ToSchema, ToJSON, FromJSON)

data DANTypeValidationReq = DANTypeValidationReq
  { rideId :: Text,
    driverId :: Value,
    rideNotificationStatus :: RideNotificationStatus,
    apiKey :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

driverArrivalNotfHandler :: DANTypeValidationReq -> Flow APISuccess
driverArrivalNotfHandler (DANTypeValidationReq rideId _ status _) = do
  ride <- B.runInReplica $ QRide.findById (Id rideId) >>= fromMaybeM (RideDoesNotExist rideId)

  when (ride.status == COMPLETED || ride.status == CANCELLED) $
    throwError $ RideInvalidStatus ("Cannot track this ride: " <> pack (show ride.status))

  booking <- B.runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)

  case status of
    DRIVER_ON_THE_WAY -> Notify.notifyDriverOnTheWay booking.riderId booking.tripCategory
    DRIVER_REACHING -> Notify.notifyDriverReaching booking.riderId booking.tripCategory ride.otp ride.vehicleNumber
    DRIVER_REACHED -> Notify.notifyDriverHasReached booking.riderId booking.tripCategory ride.otp ride.vehicleNumber
    _ -> throwError $ InvalidRequest "Unexpected ride notification status"

  pure Kernel.Types.APISuccess.Success
