{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverEarningsScreen.Transformer where

import Prelude
import Services.API as API

getEventName :: API.DriverCoinsFunctionType -> String
getEventName event = 
    case event of 
        API.OneOrTwoStarRating -> "Bad rating by Customer"
        API.RideCompleted -> "Ride Completed"
        API.FiveStarRating -> "Good rating by Customer"
        API.BookingCancellation -> "Ride Cancellation"
        API.CustomerReferral -> "Customer Referral"
        API.DriverReferral -> "Driver Referral"
        API.EightPlusRidesInOneDay -> "8+ Rides in a day"
        API.PurpleRideCompleted -> "Purple Ride Completed"
        API.LeaderBoardTopFiveHundred -> "Top 500 in weekly LeaderBoard"
        API.TrainingCompleted -> "Training Completed"
