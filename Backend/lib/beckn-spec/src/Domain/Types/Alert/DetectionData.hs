module Domain.Types.Alert.DetectionData where

import Data.Aeson hiding (Success)
import EulerHS.Prelude
import Kernel.External.Maps.Types
import Kernel.Prelude

data DetectionData
  = OverSpeedingDetection OverSpeedingDetectionData
  | StoppedDetection StoppedDetectionData
  | SkippedWaitingStopDetection SkippedWaitingStopDetectionData
  | MissedStopDetection MissedStopDetectionData
  | RouteDeviationDetection RouteDeviationDetectionData
  | OppositeDirectionDetection OppositeDirectionDetectionData
  | TripNotStartedDetection TripNotStartedDetectionData
  | SafetyCheckDetection SafetyCheckDetectionData
  | RideStopReachedDetection RideStopReachedDetectionData
  deriving (Show, Eq, Ord, Read, Generic, ToSchema)

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

data OppositeDirectionDetectionData = OppositeDirectionDetectionData
  { location :: LatLong
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data TripNotStartedDetectionData = TripNotStartedDetectionData
  { location :: LatLong
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data SafetyCheckDetectionData = SafetyCheckDetectionData
  { location :: LatLong
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data RideStopReachedDetectionData = RideStopReachedDetectionData
  { location :: LatLong,
    stopName :: Text,
    stopCode :: Text,
    stopIndex :: Int,
    reachedAt :: UTCTime
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

instance FromJSON DetectionData where
  parseJSON = withObject "DetectionData" $ \obj ->
    ( OverSpeedingDetection <$> obj .: "overSpeedingDetection"
    )
      <|> ( StoppedDetection <$> obj .: "stoppedDetection"
          )
      <|> ( SkippedWaitingStopDetection <$> obj .: "skippedWaitingStopDetection"
          )
      <|> ( MissedStopDetection <$> obj .: "missedStopDetection"
          )
      <|> ( RouteDeviationDetection <$> obj .: "routeDeviationDetection"
          )
      <|> ( OppositeDirectionDetection <$> obj .: "oppositeDirectionDetection"
          )
      <|> ( TripNotStartedDetection <$> obj .: "tripNotStartedDetection"
          )
      <|> ( SafetyCheckDetection <$> obj .: "safetyCheckDetection"
          )
      <|> ( RideStopReachedDetection <$> obj .: "rideStopReachedDetection"
          )

instance ToJSON DetectionData where
  toJSON = \case
    OverSpeedingDetection data' ->
      object ["overSpeedingDetection" .= data']
    StoppedDetection data' ->
      object ["stoppedDetection" .= data']
    SkippedWaitingStopDetection data' ->
      object ["skippedWaitingStopDetection" .= data']
    MissedStopDetection data' ->
      object ["missedStopDetection" .= data']
    RouteDeviationDetection data' ->
      object ["routeDeviationDetection" .= data']
    OppositeDirectionDetection data' ->
      object ["oppositeDirectionDetection" .= data']
    TripNotStartedDetection data' ->
      object ["tripNotStartedDetection" .= data']
    SafetyCheckDetection data' ->
      object ["safetyCheckDetection" .= data']
    RideStopReachedDetection data' ->
      object ["rideStopReachedDetection" .= data']
