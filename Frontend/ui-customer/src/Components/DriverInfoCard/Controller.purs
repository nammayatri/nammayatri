module Components.DriverInfoCard.Controller where

import Components.PrimaryButton as PrimaryButtonController
import Components.SourceToDestination as SourceToDestinationController
import Screens.Types(Stage)

data Action = NoAction
            | Support 
            | PrimaryButtonAC PrimaryButtonController.Action
            | SourceToDestinationAC SourceToDestinationController.Action
            | CancelRide DriverInfoCardState
            | LocationTracking
            | OpenEmergencyHelp

type DriverInfoCardState = 
  { props :: DriverInfoCardProps
  , data :: DriverInfoCardData
  }

type DriverInfoCardProps = 
  {
    currentStage :: Stage,
    trackingEnabled :: Boolean
  }

type DriverInfoCardData = 
  { otp :: String
  , driverName :: String
  , eta :: Int
  , vehicleDetails :: String 
  , registrationNumber :: String 
  , rating :: Number
  , startedAt :: String 
  , endedAt :: String
  , source :: String 
  , destination :: String
  , rideId :: String
  , price :: Int
  , sourceLat :: Number
  , sourceLng :: Number
  , destinationLat :: Number
  , destinationLng :: Number
  , driverLat :: Number
  , driverLng :: Number
  , distance :: Int
  , waitingTime :: String
  , driverArrived :: Boolean
  , estimatedDistance :: String
  , driverArrivalTime :: Int
  }
