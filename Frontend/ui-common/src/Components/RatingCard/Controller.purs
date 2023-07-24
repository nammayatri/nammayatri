{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RatingCard.Controller where

import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination as SourceToDestination
import Data.Maybe

data Action = NoAction
            | BackPressed
            | PrimaryButtonAC PrimaryButton.Action
            | Rating Int
            | SourceToDestinationAC SourceToDestination.Action
            | SkipButtonAC PrimaryButton.Action
            | FeedbackChanged String
            | OnClose
            | OnBackgroundClick



type RatingCardState = 
  { data :: RatingCardData
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
  , title :: String
  }

config :: RatingCardState
config = {
     data :{
      rideId : ""
    , rating : 0
    , driverName : ""
    , finalAmount : 0
    , rideStartTime : ""
    , rideEndTime : ""
    , source : ""
    , destination : ""
    , rideStartDate : ""
    , vehicleNumber : ""
    , status : ""
    , shortRideId : ""
    , bookingId : ""
    , rideEndTimeUTC : ""
    , dateDDMMYY : ""
    , offeredFare : 0
    , distanceDifference : 0
    , feedback : ""
    , title : ""
  }
}