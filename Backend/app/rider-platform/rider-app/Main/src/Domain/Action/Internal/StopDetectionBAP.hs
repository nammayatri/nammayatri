module Domain.Action.Internal.StopDetectionBAP where

import qualified BecknV2.OnDemand.Enums as Enums
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Person as DP
import Domain.Types.Ride
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Trip as DTC
import EulerHS.Prelude
import qualified Kernel.External.Notification as Notification
import Kernel.Types.APISuccess
import Kernel.Types.Error
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Notifications as Notify

data StopDetectionBAPReq = StopDetectionBAPReq
  { location :: LatLong,
    rideId :: Id DRide.Ride,
    driverId :: Id DP.Person,
    safetyAlertType :: Enums.SafetyReasonCode
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

stopDetectionBAP :: StopDetectionBAPReq -> Flow APISuccess
stopDetectionBAP StopDetectionBAPReq {..} = do
  logDebug $ "Stop detected in BAP for driverId:" <> driverId.getId
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  case ride.status of
    DRide.INPROGRESS -> do
        case booking.tripCategory of
            DTC.Rental _ -> logDebug $ "Skipping safety alert for rental ride with id" <> rideId.getId
            DTC.InterCity _ _ -> logDebug $ "Skipping safety alert for intercity ride with id" <> rideId.getId
            _ -> Notify.notifySafetyAlert booking safetyAlertType
    _ -> logDebug $ "Skipping safety alert for ride with id" <> rideId.getId
  pure Success
