module Domain.Action.Internal.DriverArrivalNotf where

import Data.Aeson
import Data.Text (pack)
import Domain.Types.Ride
import Environment
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Error
import qualified Lib.JourneyLeg.Types as LJT
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingExtra as QRBE
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
    rideNotificationStatus :: RideNotificationStatus
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

driverArrivalNotfHandler :: DANTypeValidationReq -> Flow APISuccess
driverArrivalNotfHandler (DANTypeValidationReq bppRideId _ status) = do
  ride <- B.runInReplica $ QRide.findByBPPRideId (Id bppRideId) >>= fromMaybeM (RideDoesNotExist bppRideId)
  when (ride.status == COMPLETED || ride.status == CANCELLED) $
    throwError $ RideInvalidStatus ("Cannot track this ride: " <> pack (show ride.status))
  booking <- B.runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  case status of
    DRIVER_ON_THE_WAY -> do
      QRBE.updateJourneyLegStatus (Just LJT.OnTheWay) booking.id
      Notify.notifyDriverOnTheWay booking.riderId booking.tripCategory ride
    DRIVER_REACHING -> do
      QRBE.updateJourneyLegStatus (Just LJT.Arriving) booking.id
      Notify.notifyDriverReaching booking.riderId booking.tripCategory ride.otp ride.vehicleNumber ride
    DRIVER_REACHED -> do
      QRBE.updateJourneyLegStatus (Just LJT.Arrived) booking.id
      Notify.notifyDriverHasReached booking.riderId booking.tripCategory ride.otp ride.vehicleNumber ride.vehicleColor ride.vehicleModel ride.vehicleVariant
    _ -> throwError $ InvalidRequest "Unexpected ride notification status"

  pure Kernel.Types.APISuccess.Success
