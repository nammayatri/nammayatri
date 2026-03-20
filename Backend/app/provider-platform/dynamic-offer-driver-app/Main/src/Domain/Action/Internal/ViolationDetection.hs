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
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Id
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
        existingStops <- Redis.safeGet (mkReachedStopKey rideId)
        case existingStops of
          Just existingList -> do
            unless (location `elem` existingList) $ do
              let updatedList = existingList ++ [location]
              Redis.setExp (mkReachedStopKey rideId) updatedList 86400
          Nothing -> do
            Redis.setExp (mkReachedStopKey rideId) [location] 86400
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
