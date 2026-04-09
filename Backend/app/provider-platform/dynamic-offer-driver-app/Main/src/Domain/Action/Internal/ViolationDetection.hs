module Domain.Action.Internal.ViolationDetection
  ( ViolationDetectionReq (..),
    mkReachedStopsKey,
    mkProcessedStopsKey,
    addReachedStop,
    violationDetection,
    ReachedStopInfo (..),
  )
where

import Data.Aeson as Aeson hiding (Success)
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Domain.Types.Alert
import Domain.Types.Alert.DetectionData as DTD
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.External.Maps.Types (LatLong)
import Kernel.Storage.Hedis (HedisFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import Kernel.Types.Id
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
        let nowTs = floor $ utcTimeToPOSIXSeconds now
        addReachedStop rideId (stopIndex + 1) location nowTs
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

data ReachedStopInfo = ReachedStopInfo
  { location :: LatLong,
    stopIndex :: Int,
    timestamp :: Int64
  }
  deriving (Generic, FromJSON, ToJSON, Show, Read)

mkReachedStopsKey :: Id DRide.Ride -> Text
mkReachedStopsKey rideId = "ride:" <> rideId.getId <> ":reachedStops"

mkProcessedStopsKey :: Id DRide.Ride -> Text
mkProcessedStopsKey rideId = "ride:" <> rideId.getId <> ":processedStops"

addReachedStop :: (HedisFlow m env) => Id DRide.Ride -> Int -> LatLong -> Int64 -> m ()
addReachedStop rideId stopIndex location timestamp = do
  let stopInfo = ReachedStopInfo {location = location, stopIndex = stopIndex, timestamp = timestamp}
      field = T.pack (show stopIndex)
      key = mkReachedStopsKey rideId
  void $ Redis.hSetExp key field stopInfo 86400
