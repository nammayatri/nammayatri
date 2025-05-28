module Domain.Action.Internal.StopDetection where

import qualified BecknV2.OnDemand.Enums as Enums
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Person as DP
import Domain.Types.Ride
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Trip as DTC
import Environment
import EulerHS.Prelude
import Kernel.External.Maps.Types
import qualified Kernel.External.Notification as Notification
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified SharedLogic.CallBAP as BP
import qualified Storage.Cac.TransporterConfig as CCT
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Vehicle as QVehicle
import Tools.Error
import qualified Tools.Notifications as Notify

data StopDetectionReq = StopDetectionReq
  { rideId :: Id DRide.Ride,
    driverId :: Id DP.Person,
    location :: LatLong
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

stopDetection :: StopDetectionReq -> Flow APISuccess
stopDetection StopDetectionReq {..} = do
  logDebug $ "Stopdetected for driverId:" <> driverId.getId
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  transporterConfig <- CCT.findByMerchantOpCityId booking.merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
  let distance = distanceBetweenInMeters location (LatLong booking.fromLocation.lat booking.fromLocation.lon)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  let isValidRideForStop = (ride.status == INPROGRESS || ride.status == NEW) && not booking.isScheduled

  when (isValidRideForStop && distance >= transporterConfig.minDistanceForStopFcm) $ do
    oldStopCount :: Maybe Int <- Redis.safeGet $ mkStopCountRedisKey rideId.getId
    let currStopCount = 1 + fromMaybe 0 oldStopCount
    Redis.setExp (mkStopCountRedisKey ride.id.getId) currStopCount 1200 --20 mins
    vehicleServiceTier <- CQVST.findByServiceTierTypeAndCityIdInRideFlow booking.vehicleServiceTier booking.merchantOperatingCityId booking.configInExperimentVersions >>= fromMaybeM (VehicleServiceTierNotFound (show booking.vehicleServiceTier))
    case (vehicleServiceTier.stopFcmThreshold, vehicleServiceTier.stopFcmSuppressCount) of
      (Just threshold, Just suppressCount) -> do
        let condition = sendNotificationCondition threshold suppressCount currStopCount
        case ride.status of
          NEW -> do
            mbMerchantPN <- CPN.findMatchingMerchantPN booking.merchantOperatingCityId "STOP_DETECTION_KEY" Nothing Nothing driver.language Nothing
            case (condition, mbMerchantPN) of
              (True, Just merchantPN) -> do
                sendStopDetectionAlert booking.merchantOperatingCityId merchantPN driver
              _ -> do logDebug $ "Either condition not met or no merchant PN for driver with id" <> driverId.getId
          INPROGRESS -> do
            vehicle <- QVehicle.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
            case condition of
              True -> case booking.tripCategory of
                DTC.Rental _ -> logDebug $ "Skipping safety alert for rental ride with id" <> rideId.getId
                DTC.InterCity _ _ -> logDebug $ "Skipping safety alert for intercity ride with id" <> rideId.getId
                _ -> BP.sendSafetyAlertToBAP booking ride Enums.RIDE_STOPPAGE driver vehicle
              _ -> logDebug $ "Either condition not met or no merchant PN for driver with id" <> driverId.getId
          _ -> logDebug $ "Ride status for rideId:" <> rideId.getId
      _ -> logDebug $ "Configs are empty to send stop detection alert to driver with id" <> driverId.getId

  pure Success
  where
    sendNotificationCondition threshold suppressCount currStopCount = (currStopCount >= threshold) && (((currStopCount - threshold) `mod` suppressCount) == 0)
    sendStopDetectionAlert merchantOperatingCityId merchantPN driver = do
      Notify.driverStopDetectionAlert merchantOperatingCityId Notification.DRIVER_STOP_DETECTED merchantPN.title merchantPN.body driver driver.deviceToken
      logDebug $ "Successfully sent notification of Stopdetected to driverId" <> driverId.getId

mkStopCountRedisKey :: Text -> Text
mkStopCountRedisKey rideId = "stopCountRide-" <> rideId
