module Domain.Types.Alert.AlertRequestData where

import Data.Text
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.Common

data AlertRequestData
  = EndRide EndRideData
  | OverSpeeding OverSpeedingData
  | Stopped StoppedData
  | SkippedWaitingStop SkippedWaitingStopData
  | MissedStop MissedStopData
  | WrongStartStop WrongStartStopData
  | RouteDeviation RouteDeviationData
  | OppositeDirection OppositeDirectionData
  | TripNotStarted TripNotStartedData
  | SafetyCheck SafetyCheckData
  | RideStopReached RideStopReachedData
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data EndRideData = EndRideData
  { driverMobileNumber :: Maybe Text,
    driverName :: Text,
    lat :: Double,
    lon :: Double,
    tripCode :: Maybe Text,
    tripTransactionId :: Text,
    vehicleRegistrationNumber :: Text,
    distance :: Maybe HighPrecMeters
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema, Eq, Ord, Read)

data OverSpeedingData = OverSpeedingData {driverMobileNumber :: Maybe Text, driverName :: Text, location :: LatLong, speed :: Double}
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data StoppedData = StoppedData
  { driverMobileNumber :: Maybe Text,
    driverName :: Text,
    location :: LatLong
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data SkippedWaitingStopData = SkippedWaitingStopData
  { driverMobileNumber :: Maybe Text,
    driverName :: Text,
    location :: LatLong,
    stopName :: Text
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data MissedStopData = MissedStopData
  { driverMobileNumber :: Maybe Text,
    driverName :: Text,
    location :: LatLong,
    stopName :: Text
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data WrongStartStopData = WrongStartStopData
  { driverMobileNumber :: Maybe Text,
    driverName :: Text,
    location :: LatLong,
    stopName :: Text,
    distance :: Maybe HighPrecMeters
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data RouteDeviationData = RouteDeviationData
  { driverMobileNumber :: Maybe Text,
    driverName :: Text,
    location :: LatLong,
    distance :: Double
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data OppositeDirectionData = OppositeDirectionData
  { driverMobileNumber :: Maybe Text,
    driverName :: Text,
    location :: LatLong
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data TripNotStartedData = TripNotStartedData
  { driverMobileNumber :: Maybe Text,
    driverName :: Text,
    location :: LatLong
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data SafetyCheckData = SafetyCheckData
  { driverMobileNumber :: Maybe Text,
    driverName :: Text,
    location :: LatLong
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data RideStopReachedData = RideStopReachedData
  { location :: LatLong,
    stopName :: Text,
    stopCode :: Text,
    stopIndex :: Int,
    reachedAt :: UTCTime
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)
