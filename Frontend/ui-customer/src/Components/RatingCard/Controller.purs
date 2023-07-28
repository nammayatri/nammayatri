{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RatingCard.Controller where

import Screens.Types(Stage, ZoneType(..))
import Components.PrimaryButton as PrimaryButton
import Components.FareBreakUp as FareBreakUp
import Components.SourceToDestination as SourceToDestination
import Data.Maybe
import Services.API(FeedbackAnswer)
import Common.Types.App (LazyCheck(..))
import Language.Strings (getString)
import Language.Types (STR(..))

data Action = NoAction
            | BackPressed
            | PrimaryButtonAC PrimaryButton.Action
            | Rating Int
            | FareBreakUpAC FareBreakUp.Action
            | SourceToDestinationAC SourceToDestination.Action
            | SkipButtonAC PrimaryButton.Action
            | FeedbackChanged String
            | SelectPill String String

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
  , feedbackList :: Array FeedbackAnswer
  }

type FeedbackItem = { 
    id :: String
  , text :: String
  }

feedbackPillData :: LazyCheck -> Array (Array (Array FeedbackItem)) 
feedbackPillData lazyCheck = [feedbackPillDataWithRating1 Language, feedbackPillDataWithRating2 Language, feedbackPillDataWithRating3 Language, feedbackPillDataWithRating4 Language, feedbackPillDataWithRating5 Language]

feedbackPillDataWithRating1 :: LazyCheck -> Array (Array FeedbackItem)
feedbackPillDataWithRating1 lazycheck = [
  [{id : "6", text : getString RUDE_DRIVER},
  {id : "1", text : getString FELT_UNSAFE},
  {id : "1", text : getString TOO_MANY_CALLS}],
  [{id : "6", text : getString RECKLESS_DRIVING},
  {id : "6", text : getString DRIVER_CHARGED_MORE}],
  [{id : "1", text : getString LATE_DROP_OFF},
  {id : "1", text : getString LATE_PICK_UP}]
]

feedbackPillDataWithRating2 :: LazyCheck -> Array (Array FeedbackItem)
feedbackPillDataWithRating2 lazycheck = [
  [{id : "7", text : getString RUDE_DRIVER},
  {id : "2", text : getString FELT_UNSAFE},
  {id : "2", text : getString TOO_MANY_CALLS}],
  [{id : "7", text : getString RECKLESS_DRIVING},
  {id : "7", text : getString DRIVER_CHARGED_MORE}],
  [{id : "2", text : getString LATE_DROP_OFF},
  {id : "2", text : getString LATE_PICK_UP}]
]

feedbackPillDataWithRating3 :: LazyCheck -> Array (Array FeedbackItem)
feedbackPillDataWithRating3 lazycheck = [
  [{id : "8", text : getString UNPROFESSIONAL_DRIVER},
  {id : "8", text : getString RASH_DRIVING}],
  [{id : "8", text : getString DRIVER_CHARGED_MORE},
  {id : "11", text : getString UNCOMFORTABLE_AUTO}],
  [{id : "3", text : getString TRIP_GOT_DELAYED},
  {id : "3", text : getString FELT_UNSAFE}]
]

feedbackPillDataWithRating4 :: LazyCheck -> Array (Array FeedbackItem)
feedbackPillDataWithRating4 lazycheck = [
  [{id : "9", text : getString POLITE_DRIVER},
  {id : "9", text : getString EXPERT_DRIVING}],
  [{id : "9", text : getString ASKED_FOR_EXTRA_FARE},
  {id : "11", text : getString UNCOMFORTABLE_AUTO}],
  [{id : "4", text : getString TRIP_GOT_DELAYED},
  {id : "4", text : getString SAFE_RIDE}]
]

feedbackPillDataWithRating5 :: LazyCheck -> Array (Array FeedbackItem)
feedbackPillDataWithRating5 lazyCheck = [
  [{id : "10", text : getString POLITE_DRIVER},
  {id : "5", text : getString EXPERT_DRIVING}],
  [{id : "12", text : getString CLEAN_AUTO},
  {id : "10", text : getString ON_TIME}],
  [{id : "10", text : getString SKILLED_NAVIGATOR},
  {id : "5", text : getString SAFE_RIDE}]
]