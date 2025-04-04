module Domain.Action.Internal.ViolationDetection where

import Data.Aeson hiding (Success)
import Data.OpenApi (ToSchema)
import Domain.Types.Alert
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Environment
import EulerHS.Prelude
import Kernel.External.Encryption
import Kernel.External.Maps.Types
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
    detectionData :: DetectionData
  }
  deriving (Generic, FromJSON, ToJSON, Show, Read)

data DetectionData
  = OverSpeedingDetection OverSpeedingDetectionData
  | StoppedDetection StoppedDetectionData
  | SkippedWaitingStopDetection SkippedWaitingStopDetectionData
  | MissedStopDetection MissedStopDetectionData
  | RouteDeviationDetection RouteDeviationDetectionData
  deriving (Show, Eq, Ord, Read, Generic)

data OverSpeedingDetectionData = OverSpeedingDetectionData {location :: LatLong, speed :: Double}
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data StoppedDetectionData = StoppedDetectionData
  { location :: LatLong
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data SkippedWaitingStopDetectionData = SkippedWaitingStopDetectionData
  { location :: LatLong,
    stopName :: Text
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data MissedStopDetectionData = MissedStopDetectionData
  { location :: LatLong,
    stopName :: Text
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data WrongStartStopDetectionData = WrongStartStopDetectionData
  { location :: LatLong,
    stopName :: Text
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data RouteDeviationDetectionData = RouteDeviationDetectionData
  { location :: LatLong,
    distance :: Double
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

instance FromJSON DetectionData where
  parseJSON = withObject "DetectionData" $ \obj ->
    ( OverSpeedingDetection <$> obj .: "overSpeeding"
    )
      <|> ( StoppedDetection <$> obj .: "stopped"
          )
      <|> ( SkippedWaitingStopDetection <$> obj .: "skippedWaitingStop"
          )
      <|> ( MissedStopDetection <$> obj .: "missedStop"
          )
      <|> ( RouteDeviationDetection <$> obj .: "routeDeviation"
          )

instance ToJSON DetectionData where
  toJSON = \case
    OverSpeedingDetection data' ->
      object ["overSpeeding" .= data']
    StoppedDetection data' ->
      object ["stopped" .= data']
    SkippedWaitingStopDetection data' ->
      object ["skippedWaitingStop" .= data']
    MissedStopDetection data' ->
      object ["missedStop" .= data']
    RouteDeviationDetection data' ->
      object ["routeDeviation" .= data']

violationDetection :: ViolationDetectionReq -> Flow APISuccess
violationDetection ViolationDetectionReq {..} = do
  tripTransaction <- QTT.findByPrimaryKey (cast rideId) >>= fromMaybeM (TripTransactionNotFound rideId.getId)
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  let driverName = driver.firstName <> " " <> (fromMaybe "" driver.lastName)
  driverMobileNumber <- mapM decrypt driver.mobileNumber
  (requestTitle, requestBody, requestData) <- getAlertRequestData driverName driverMobileNumber detectionData
  void $ triggerAlertRequest driverId tripTransaction.fleetOwnerId.getId requestTitle requestBody requestData isViolated tripTransaction
  pure Success
  where
    getAlertRequestData :: Text -> Maybe Text -> DetectionData -> Flow (Text, Text, AlertRequestData)
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
