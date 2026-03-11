module Domain.Action.Internal.ViolationDetection
  ( ViolationDetectionReq (..),
    mkReachedStopKey,
    lastPair,
    isNearPoint,
    isNearLastStop,
    violationDetection,
  )
where

import Data.Aeson hiding (Success)
import Data.OpenApi (ToSchema)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Domain.Types.Alert
import Domain.Types.Alert.DetectionData as DTD
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.External.Maps.Types (LatLong)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import SharedLogic.WMB
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.TripTransaction as QTT
import Tools.Error
import qualified Tools.Notifications as TN

data ViolationDetectionReq = ViolationDetectionReq
  { rideId :: Id DRide.Ride,
    driverId :: Id DP.Person,
    isViolated :: Bool,
    detectionData :: DTD.DetectionData
  }
  deriving (Generic, FromJSON, ToJSON, Show, Read, ToSchema)

violationDetection :: ViolationDetectionReq -> Flow APISuccess
violationDetection ViolationDetectionReq {..} = do
  mbTripTransaction <- QTT.findByPrimaryKey (cast rideId)
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  let driverName = driver.firstName <> " " <> (fromMaybe "" driver.lastName)
  driverMobileNumber <- mapM decrypt driver.mobileNumber
  (requestTitle, requestBody, requestData) <- getAlertRequestData driverName driverMobileNumber detectionData
  case detectionData of
    RideStopReachedDetection RideStopReachedDetectionData {..} -> do
      when isViolated $ do
        now <- getCurrentTime
        let nowTs = floor $ utcTimeToPOSIXSeconds now
        existingStops <- Redis.safeGet (mkReachedStopKey rideId)
        case existingStops of
          Just stops -> do
            unless (isNearLastStop location stops) $
              Redis.setExp (mkReachedStopKey rideId) (stops ++ [(location, nowTs)]) 86400

          Nothing ->
            Redis.setExp (mkReachedStopKey rideId) [(location, nowTs)] 86400
    RouteDeviationDetection _ -> when isViolated $ notifyDriver driver "DRIVER_ROUTE_DEVIATION"
    StoppedDetection _ -> when isViolated $ notifyDriver driver "DRIVER_RIDE_STOPPAGE"
    _ -> pure ()
  whenJust mbTripTransaction $ \tripTransaction ->
    void $ triggerAlertRequest driverId tripTransaction.fleetOwnerId.getId requestTitle requestBody requestData isViolated tripTransaction
  pure Success
  where
    notifyDriver driver pnKey = do
      mbMerchantPN <- CPN.findMatchingMerchantPN driver.merchantOperatingCityId pnKey Nothing Nothing driver.language Nothing
      whenJust mbMerchantPN $ \merchantPN -> do
        let entityData = TN.NotifReq {entityId = driver.id.getId, title = merchantPN.title, message = merchantPN.body}
        TN.notifyDriverOnEvents driver.merchantOperatingCityId driver.id driver.deviceToken entityData merchantPN.fcmNotificationType

    getAlertRequestData :: Text -> Maybe Text -> DTD.DetectionData -> Flow (Text, Text, AlertRequestData)
    getAlertRequestData driverName driverMobileNumber = \case
      OverSpeedingDetection OverSpeedingDetectionData {..} -> do
        let requestTitle = "Over Speeding"
        let requestBody = "Over Speeding Detected"
        let requestData = OverSpeeding OverSpeedingData {..}
        return (requestTitle, requestBody, requestData)
      StoppedDetection StoppedDetectionData {..} -> do
        let requestTitle = "Stopped"
        let requestBody = "Stopped Detected"
        let requestData = Stopped StoppedData {..}
        return (requestTitle, requestBody, requestData)
      SkippedWaitingStopDetection SkippedWaitingStopDetectionData {..} -> do
        let requestTitle = "Skipped Waiting Stop"
        let requestBody = "Skipped Waiting Stop Detected"
        let requestData = SkippedWaitingStop SkippedWaitingStopData {..}
        return (requestTitle, requestBody, requestData)
      MissedStopDetection MissedStopDetectionData {..} -> do
        let requestTitle = "Missed Stop"
        let requestBody = "Missed Stop Detected"
        let requestData = MissedStop MissedStopData {..}
        return (requestTitle, requestBody, requestData)
      RouteDeviationDetection RouteDeviationDetectionData {..} -> do
        let requestTitle = "Route Deviation"
        let requestBody = "Route Deviation Detected"
        let requestData = RouteDeviation RouteDeviationData {..}
        return (requestTitle, requestBody, requestData)
      OppositeDirectionDetection OppositeDirectionDetectionData {..} -> do
        let requestTitle = "Opposite Direction"
        let requestBody = "Opposite Direction Detected"
        let requestData = OppositeDirection OppositeDirectionData {..}
        return (requestTitle, requestBody, requestData)
      TripNotStartedDetection TripNotStartedDetectionData {..} -> do
        let requestTitle = "Trip Not Started"
        let requestBody = "Trip Not Started Detected"
        let requestData = TripNotStarted TripNotStartedData {..}
        return (requestTitle, requestBody, requestData)
      SafetyCheckDetection SafetyCheckDetectionData {..} -> do
        let requestTitle = "Safety Check"
        let requestBody = "Safety Check Detected"
        let requestData = SafetyCheck SafetyCheckData {..}
        return (requestTitle, requestBody, requestData)
      RideStopReachedDetection RideStopReachedDetectionData {..} -> do
        -- NEW
        let requestTitle = "Ride Stop Reached"
        let requestBody = "Driver has reached ride stop: " <> stopName
        let requestData = RideStopReached RideStopReachedData {..}
        -- You'll need to define this in Alert types
        return (requestTitle, requestBody, requestData)

mkReachedStopKey :: Id DRide.Ride -> Text
mkReachedStopKey rideId = "add_stop_ride_reached_stop:" <> rideId.getId

lastPair :: [(a, b)] -> Maybe (a, b)
lastPair = foldl' (\_ x -> Just x) Nothing

isNearPoint :: LatLong -> LatLong -> Bool
isNearPoint a b = highPrecMetersToMeters (distanceBetweenInMeters a b) <= 10

isNearLastStop :: LatLong -> [(LatLong, Int64)] -> Bool
isNearLastStop pt stops = case lastPair stops of
  Just (prevLL, _) -> isNearPoint pt prevLL
  Nothing          -> False
