module Domain.Action.Internal.ViolationDetection where

import qualified BecknV2.OnDemand.Enums as Enums
import Data.OpenApi (ToSchema)
import Domain.Types.Alert.DetectionData as DTD
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideStatus as DRide
import qualified Domain.Types.Trip as DTC
import Environment
import EulerHS.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, logDebug)
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide
import qualified Tools.Notifications as Notify

data ViolationDetectionReq = ViolationDetectionReq
  { rideId :: Id DRide.BPPRide,
    driverId :: Id DP.Person,
    isViolated :: Bool,
    detectionData :: DTD.DetectionData
  }
  deriving (Generic, FromJSON, ToJSON, Show, Read, ToSchema)

violationDetection :: ViolationDetectionReq -> Flow APISuccess
violationDetection ViolationDetectionReq {..} = do
  logDebug $ "Stop detected in BAP for driverId:" <> driverId.getId
  ride <- QRide.findByBPPRideId rideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> rideId.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  case detectionData of
    OverSpeedingDetection _ -> do
      logDebug $ "Over speeding detected for driverId:" <> driverId.getId
    StoppedDetection _ -> do
      logDebug $ "Stopped detected for driverId:" <> driverId.getId
    SkippedWaitingStopDetection _ -> do
      logDebug $ "Skipped waiting stop detected for driverId:" <> driverId.getId
    MissedStopDetection _ -> do
      logDebug $ "Missed stop detected for driverId:" <> driverId.getId
    RouteDeviationDetection _ -> do
      logDebug $ "Route deviation detected for driverId:" <> driverId.getId
    OppositeDirectionDetection _ -> do
      logDebug $ "Opposite direction detected for driverId:" <> driverId.getId
    TripNotStartedDetection _ -> do
      logDebug $ "Trip not started detected for driverId:" <> driverId.getId
    SafetyCheckDetection _ -> do
      logDebug $ "Safety check detected for driverId:" <> driverId.getId
      case ride.status of
        DRide.INPROGRESS -> do
          if isViolated
            then do
              case booking.tripCategory of
                Just (DTC.Rental _) -> logDebug $ "Skipping safety alert for rental ride with id" <> rideId.getId
                Just (DTC.InterCity _ _) -> logDebug $ "Skipping safety alert for intercity ride with id" <> rideId.getId
                _ -> Notify.notifySafetyAlert booking (show $ Enums.RIDE_STOPPAGE)
            else do
              logDebug $ "Anti-Violation detected for ride with id" <> rideId.getId
        _ -> logDebug $ "Skipping safety alert for ride with id" <> rideId.getId
    RideStopReachedDetection _ -> do
      logDebug $ "Ride stop reached detected for ride:" <> rideId.getId

  pure Success
