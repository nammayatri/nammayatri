module Components.RatingCard.Controller where

import Screens.Types(Stage)
import Components.PrimaryButton as PrimaryButton
import Components.FareBreakUp as FareBreakUp
import Components.SourceToDestination as SourceToDestination
import Data.Maybe

data Action = NoAction
            | BackPressed 
            | PrimaryButtonAC PrimaryButton.Action
            | Rating Int
            | FareBreakUpAC FareBreakUp.Action 
            | SourceToDestinationAC SourceToDestination.Action
            | SkipButtonAC PrimaryButton.Action
            | FeedbackChanged String

type RatingCardState = 
  { props :: RatingCardProps
  , data :: RatingCardData
  }

type RatingCardProps = 
  {
    currentStage :: Stage
  , estimatedDistance :: Maybe Int
  , enableFeedback :: Boolean 
  , showFareBreakUp :: Boolean 
  }

type RatingCardData = 
  { 
    rating :: Int
  , driverName :: String
  , rideId :: String
  , finalAmount :: Int
  , rideStartTime :: String 
  , rideStartDate :: String 
  , rideEndTime :: String 
  , source :: String
  , destination :: String 
  , vehicleNumber :: String 
  , status :: String 
  , shortRideId :: String 
  , bookingId :: String 
  , rideEndTimeUTC :: String
  , dateDDMMYY :: String
  , offeredFare :: Int
  , distanceDifference :: Int
  , feedback :: String
  }
