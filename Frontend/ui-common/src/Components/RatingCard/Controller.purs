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
import Data.Maybe
import Common.Types.App (LazyCheck(..), FeedbackAnswer)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Types
import Components.PrimaryButton as PrimaryButto
import PrestoDOM (Accessiblity(..))

data Action = NoAction
            | BackPressed
            | PrimaryButtonAC PrimaryButton.Action
            | Rating Int
            | FeedbackChanged String
            | SelectPill String String

type RatingCardConfig = 
  { data :: RatingCardData
  , feedbackPillData :: Array (Array (Array FeedbackItem)) 
  , primaryButtonConfig :: PrimaryButton.Config
  , showProfileImg :: Boolean
  , title :: String 
  , feedbackPlaceHolder :: String 
  , showFeedbackPill :: Boolean
  , overallFeedbackArray :: Array String
  , accessibility :: Accessiblity
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
  , feedbackList :: Array FeedbackAnswer
  }

type FeedbackItem = { 
    id :: String
  , text :: String
  }

ratingCardConfig :: RatingCardConfig
ratingCardConfig = {
  data : dummyPreviousRiderating,
  feedbackPillData : [],
  primaryButtonConfig : PrimaryButton.config,
  showProfileImg : false, 
  title : "",
  feedbackPlaceHolder : "", 
  showFeedbackPill : false,
  overallFeedbackArray : [],
  accessibility : DISABLE
}


dummyPreviousRiderating :: RatingCardData
dummyPreviousRiderating = {
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
, feedbackList : []
}

