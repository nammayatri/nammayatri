module Domain.Action.Internal.StopDetectionBAP where

import qualified BecknV2.OnDemand.Enums as Enums
import Data.OpenApi (ToSchema)
import Domain.Types.Alert.DetectionData as DTD
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

data ViolationDetectionReq = ViolationDetectionReq
  { rideId :: Id DRide.Ride,
    driverId :: Id DP.Person,
    isViolated :: Bool,
    detectionData :: DTD.DetectionData
  }
  deriving (Generic, FromJSON, ToJSON, Show, Read, ToSchema)

violationDetection :: ViolationDetectionReq -> Flow APISuccess
violationDetection ViolationDetectionReq {..} = do
  logDebug $ "Stop detected in BAP for driverId:" <> driverId.getId
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  case detectionData of
    OverSpeedingDetection overSpeedingDetectionData -> do
      logDebug $ "Over speeding detected for driverId:" <> driverId.getId
    StoppedDetection stoppedDetectionData -> do
      logDebug $ "Stopped detected for driverId:" <> driverId.getId
    SkippedWaitingStopDetection skippedWaitingStopDetectionData -> do
      logDebug $ "Skipped waiting stop detected for driverId:" <> driverId.getId
    MissedStopDetection missedStopDetectionData -> do
      logDebug $ "Missed stop detected for driverId:" <> driverId.getId
    RouteDeviationDetection routeDeviationDetectionData -> do
      logDebug $ "Route deviation detected for driverId:" <> driverId.getId
    OppositeDirectionDetection oppositeDirectionDetectionData -> do
      logDebug $ "Opposite direction detected for driverId:" <> driverId.getId
    TripNotStartedDetection tripNotStartedDetectionData -> do
      logDebug $ "Trip not started detected for driverId:" <> driverId.getId
    SafetyCheckDetection safetyCheckDetectionData -> do
      logDebug $ "Safety check detected for driverId:" <> driverId.getId
      case ride.status of
        DRide.INPROGRESS -> do
          if isViolated
            then do
              case booking.tripCategory of
                DTC.Rental _ -> logDebug $ "Skipping safety alert for rental ride with id" <> rideId.getId
                DTC.InterCity _ _ -> logDebug $ "Skipping safety alert for intercity ride with id" <> rideId.getId
                _ -> Notify.notifySafetyAlert booking safetyAlertType
            else do
              logDebug $ "Anti-Violation detected for ride with id" <> rideId.getId
        _ -> logDebug $ "Skipping safety alert for ride with id" <> rideId.getId

  pure Success
