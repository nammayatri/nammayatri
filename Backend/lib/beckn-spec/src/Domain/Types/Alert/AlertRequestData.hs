module Domain.Types.Alert.AlertRequestData where

import Data.Text
import Kernel.External.Maps.Types
import Kernel.Prelude

data AlertRequestData
  = EndRide EndRideData
  | OverSpeeding OverSpeedingData
  | Stopped StoppedData
  | SkippedWaitingStop SkippedWaitingStopData
  | MissedStop MissedStopData
  | WrongStartStop WrongStartStopData
  | RouteDeviation RouteDeviationData
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data EndRideData = EndRideData
  { driverMobileNumber :: Maybe Text,
    driverName :: Text,
    lat :: Double,
    lon :: Double,
    tripCode :: Maybe Text,
    tripTransactionId :: Text,
    vehicleRegistrationNumber :: Text
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
    stopName :: Text
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data RouteDeviationData = RouteDeviationData
  { driverMobileNumber :: Maybe Text,
    driverName :: Text,
    location :: LatLong,
    distance :: Double
  }
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)
