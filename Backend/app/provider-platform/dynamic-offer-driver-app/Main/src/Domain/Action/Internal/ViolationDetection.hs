module Domain.Action.Internal.ViolationDetection where

import Data.Aeson hiding (Success)
import Data.OpenApi (ToSchema)
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
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.TripTransaction as QTT
import Tools.Error

data ViolationDetectionReq = ViolationDetectionReq
  { rideId :: Id DRide.Ride,
    driverId :: Id DP.Person,
    isViolated :: Bool,
    detectionData :: DTD.DetectionData
  }
  deriving (Generic, FromJSON, ToJSON, Show, Read, ToSchema)

violationDetection :: ViolationDetectionReq -> Flow APISuccess
violationDetection ViolationDetectionReq {..} = do
  tripTransaction <- QTT.findByPrimaryKey (cast rideId) >>= fromMaybeM (TripTransactionNotFound rideId.getId)
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  let driverName = driver.firstName <> " " <> (fromMaybe "" driver.lastName)
  driverMobileNumber <- mapM decrypt driver.mobileNumber
  (requestTitle, requestBody, requestData) <- getAlertRequestData driverName driverMobileNumber detectionData
  case detectionData of
    RideStopReachedDetection RideStopReachedDetectionData {..} -> do
      when isViolated $ do
        now <- getCurrentTime
        existingStops <- getReachedStopsWithFallback rideId
        let isNearExisting ll = any (\existingLL -> highPrecMetersToMeters (distanceBetweenInMeters ll existingLL) <= 10) (map fst existingStops)
        unless (isNearExisting location) $ do
          let updatedList = existingStops ++ [(location, now)]
          Redis.setExp (mkReachedStopKey rideId) updatedList 86400
    _ -> pure ()
  void $ triggerAlertRequest driverId tripTransaction.fleetOwnerId.getId requestTitle requestBody requestData isViolated tripTransaction
  pure Success
  where
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

-- Helper function to get reached stops with fallback for backward compatibility
getReachedStopsWithFallback :: (Redis.HedisFlow m env, MonadTime m, Log m) => Id DRide.Ride -> m [(LatLong, UTCTime)]
getReachedStopsWithFallback rideId = do
  let key = mkReachedStopKey rideId
  -- Try new format first: [(LatLong, UTCTime)]
  mbNewFormat <- Redis.runInMultiCloudRedisMaybeResult $ Redis.get key
  case mbNewFormat of
    Just stops -> return stops
    Nothing -> do
      -- Fall back to old format: [LatLong]
      mbOldFormat <- Redis.runInMultiCloudRedisMaybeResult $ Redis.get key
      case mbOldFormat of
        Just (oldStops :: [LatLong]) -> do
          logInfo $ "Migrating legacy reached stops format for rideId: " <> rideId.getId
          now <- getCurrentTime
          return $ map (,now) oldStops
        Nothing -> return []
