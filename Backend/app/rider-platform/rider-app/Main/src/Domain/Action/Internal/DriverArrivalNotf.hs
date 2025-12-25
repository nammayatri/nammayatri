module Domain.Action.Internal.DriverArrivalNotf where

import Data.Aeson
import Data.Text (pack)
import qualified Domain.Action.Internal.PickupInstructionHandler as PIHandler
import Domain.Types.RideStatus
import Environment
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyModule.State.Types as JMState
import qualified Lib.JourneyModule.State.Utils as JMState
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Notifications as Notify

data RideNotificationStatus
  = IDLE
  | DRIVER_ON_THE_WAY
  | DRIVER_PICKUP_INSTRUCTION
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
driverArrivalNotfHandler (DANTypeValidationReq bppRideId driverIdValue status) = do
  now <- getCurrentTime
  ride <- B.runInReplica $ QRide.findByBPPRideId (Id bppRideId) >>= fromMaybeM (RideDoesNotExist bppRideId)
  when (ride.status == COMPLETED || ride.status == CANCELLED) $
    throwError $ RideInvalidStatus ("Cannot track this ride: " <> pack (show ride.status))
  booking <- B.runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  mbJourneyLeg <- QJourneyLeg.findByLegSearchId (Just booking.transactionId)
  case status of
    DRIVER_ON_THE_WAY -> do
      whenJust mbJourneyLeg $ \journeyLeg -> JMState.setJourneyLegTrackingStatus journeyLeg Nothing JMState.Arriving now
      Notify.notifyDriverOnTheWay booking.riderId booking.tripCategory ride
    DRIVER_PICKUP_INSTRUCTION -> do
      PIHandler.handlePickupInstruction ride booking driverIdValue
    DRIVER_REACHING -> do
      whenJust mbJourneyLeg $ \journeyLeg -> JMState.setJourneyLegTrackingStatus journeyLeg Nothing JMState.AlmostArrived now
      Notify.notifyDriverReaching booking.riderId booking.tripCategory ride.otp ride.vehicleNumber ride
    DRIVER_REACHED -> do
      whenJust mbJourneyLeg $ \journeyLeg -> JMState.setJourneyLegTrackingStatus journeyLeg Nothing JMState.Arrived now
      Notify.notifyDriverHasReached booking.riderId booking.tripCategory ride.otp ride.vehicleNumber ride.vehicleColor ride.vehicleModel ride.vehicleVariant
    _ -> throwError $ InvalidRequest "Unexpected ride notification status"

  pure Kernel.Types.APISuccess.Success
