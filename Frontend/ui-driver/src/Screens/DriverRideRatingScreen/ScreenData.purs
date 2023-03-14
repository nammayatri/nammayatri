module Screens.DriverRideRatingScreen.ScreenData where

import Screens.Types
import Data.Maybe

initData :: DriverRideRatingScreenState
initData = {
  data: 
  { rating : 5
  , rideId : ""
  , feedback : ""
  , customerName : "Customer"
  , activeFeedBackOption : Nothing
  , selectedFeedbackOption : ""
  },
  
  props: { }
}


feedbackSuggestionArray ::  Array (Array FeedbackSuggestions) 
feedbackSuggestionArray = [[CUSTOMER_RUDE_BEHAVIOUR, LONG_WAIT_TIME], [DIDNT_COME_TO_PICUP, CUSTOMER_RUDE_BEHAVIOUR], [LONG_WAIT_TIME]]