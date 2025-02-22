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
  = Idle
  | DriverOnTheWay
  | DriverReaching
  | DriverReached
  deriving (Show, Eq, Generic, ToSchema)

instance FromJSON RideNotificationStatus where
  parseJSON = withText "RideNotificationStatus" $ \t -> case t of
    "IDLE" -> pure Idle
    "DRIVER_ON_THE_WAY" -> pure DriverOnTheWay
    "DRIVER_REACHING" -> pure DriverReaching
    "DRIVER_REACHED" -> pure DriverReached
    _ -> fail "Invalid RideNotificationStatus"

instance ToJSON RideNotificationStatus where
  toJSON Idle = String "IDLE"
  toJSON DriverOnTheWay = String "DRIVER_ON_THE_WAY"
  toJSON DriverReaching = String "DRIVER_REACHING"
  toJSON DriverReached = String "DRIVER_REACHED"

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
    DriverOnTheWay -> Notify.notifyDriverOnTheWay booking.riderId booking.tripCategory
    DriverReaching -> Notify.notifyDriverReaching booking.riderId booking.tripCategory ride.otp ride.vehicleNumber
    DriverReached -> Notify.notifyDriverHasReached booking.riderId booking.tripCategory ride.otp ride.vehicleNumber
    _ -> throwError $ InvalidRequest "Unexpected ride notification status"

  pure Kernel.Types.APISuccess.Success
