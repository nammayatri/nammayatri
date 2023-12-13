{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.DriverRideRatingScreen.ScreenData where

import Screens.Types
import Data.Maybe

initData :: DriverRideRatingScreenState
initData =
  { data:
      { rating: 5
      , rideId: ""
      , feedback: ""
      , customerName: "Customer"
      , activeFeedBackOption: Nothing
      , selectedFeedbackOption: ""
      }
  , props: {}
  }

feedbackSuggestionArray :: Array (Array FeedbackSuggestions)
feedbackSuggestionArray = [ [ CUSTOMER_RUDE_BEHAVIOUR, LONG_WAIT_TIME ], [ DIDNT_COME_TO_PICUP, CUSTOMER_RUDE_BEHAVIOUR ], [ LONG_WAIT_TIME ] ]
