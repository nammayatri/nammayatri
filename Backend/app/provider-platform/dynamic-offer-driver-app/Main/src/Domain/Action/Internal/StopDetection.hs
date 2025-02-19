module Domain.Action.Internal.StopDetection where

import Data.OpenApi (ToSchema)
import qualified Domain.Types.Person as DP
import Domain.Types.Ride
import qualified Domain.Types.Ride as DRide
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
import qualified Storage.Cac.TransporterConfig as CCT
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.CachedQueries.VehicleServiceTier as CQVST
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
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
  let isValidRideForStop = ride.status == NEW && booking.isScheduled == False
  when (isValidRideForStop && distance >= transporterConfig.minDistanceForStopFcm) $ do
    oldStopCount :: Maybe Int <- Redis.safeGet $ mkStopCountDuringPickupRedisKey rideId.getId
    let currStopCount = 1 + fromMaybe 0 oldStopCount
    Redis.setExp (mkStopCountDuringPickupRedisKey ride.id.getId) currStopCount 1200 --20 mins
    vehicleServiceTier <- CQVST.findByServiceTierTypeAndCityId booking.vehicleServiceTier booking.merchantOperatingCityId >>= fromMaybeM (VehicleServiceTierNotFound (show booking.vehicleServiceTier))
    case (vehicleServiceTier.stopFcmThreshold, vehicleServiceTier.stopFcmSuppressCount) of
      (Just threshold, Just suppressCount) -> do
        let condition = sendNotificationCondition threshold suppressCount currStopCount
        driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
        mbMerchantPN <- CPN.findMatchingMerchantPN booking.merchantOperatingCityId "STOP_DETECTION_KEY" Nothing Nothing driver.language Nothing
        case (condition, mbMerchantPN) of
          (True, Just merchantPN) -> do
            sendStopDetectionAlert booking.merchantOperatingCityId merchantPN driver
          _ -> do logDebug $ "Either condition not met or no merchant PN for driver with id" <> driverId.getId
      _ -> do logDebug $ "Configs are empty to send stop detection alet to driver with id" <> driverId.getId
  pure Success
  where
    sendNotificationCondition threshold suppressCount currStopCount = (currStopCount >= threshold) && (((currStopCount - threshold) `mod` suppressCount) == 0)
    sendStopDetectionAlert merchantOperatingCityId merchantPN driver = do
      Notify.driverStopDetectionAlert merchantOperatingCityId Notification.DRIVER_STOP_DETECTED merchantPN.title merchantPN.body driver driver.deviceToken
      logDebug $ "Successfully sent notification of Stopdetected to driverId" <> driverId.getId

mkStopCountDuringPickupRedisKey :: Text -> Text
mkStopCountDuringPickupRedisKey rideId = "stopCountRide-" <> rideId
