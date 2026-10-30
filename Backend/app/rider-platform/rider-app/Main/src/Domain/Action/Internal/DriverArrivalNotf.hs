module Domain.Action.Internal.DriverArrivalNotf where

import Data.Aeson
import Data.Text (pack)
import qualified Domain.Action.Beckn.Common as Common
import qualified Domain.Action.Internal.PickupInstructionHandler as PIHandler
import qualified Domain.Types.Ride as DRide
import Domain.Types.RideStatus
import Environment
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Tools.Error

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
  ride <- B.runInReplica $ QRide.findByBPPRideId (Id bppRideId) >>= fromMaybeM (RideDoesNotExist bppRideId)
  when (ride.status == COMPLETED || ride.status == CANCELLED) $
    throwError $ RideInvalidStatus ("Cannot track this ride: " <> pack (show ride.status))
  booking <- B.runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  case status of
    DRIVER_ON_THE_WAY -> Common.updateAndNotifyDriverArrivalStatus booking ride DRide.DRIVER_ON_THE_WAY
    DRIVER_PICKUP_INSTRUCTION -> PIHandler.handlePickupInstruction ride booking driverIdValue
    DRIVER_REACHING -> Common.updateAndNotifyDriverArrivalStatus booking ride DRide.DRIVER_REACHING
    DRIVER_REACHED -> Common.updateAndNotifyDriverArrivalStatus booking ride DRide.DRIVER_REACHED
    _ -> throwError $ InvalidRequest "Unexpected ride notification status"

  pure Kernel.Types.APISuccess.Success
