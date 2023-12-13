{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.DriverRideRatingScreen.Controller where

import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress)
import Screens.Types (DriverRideRatingScreenState, FeedbackSuggestions(..))
import Components.PrimaryButton.Controller as PrimaryButtonController
import Prelude (class Show, pure, unit, ($), bind, discard)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import JBridge (hideKeyboardOnNavigation)
import PrestoDOM (Eval, continue, exit)
import Language.Strings (getString)
import Language.Types (STR(..))
import Data.Maybe

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen DRIVER_RIDE_RATING_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen DRIVER_RIDE_RATING_SCREEN)
      trackAppEndScreen appId (getScreen DRIVER_RIDE_RATING_SCREEN)
    PrimaryButtonActionController act -> case act of
      PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen DRIVER_RIDE_RATING_SCREEN) "primary_button" "on_click"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen DRIVER_RIDE_RATING_SCREEN) "primary_button" "no_action"
    FeedbackChanged value -> trackAppActionClick appId (getScreen DRIVER_RIDE_RATING_SCREEN) "in_screen" "feedback_changed"
    Rating index -> trackAppActionClick appId (getScreen DRIVER_RIDE_RATING_SCREEN) "in_screen" "rating_onclick"
    FeedBackClick feedBackOption -> trackAppActionClick appId (getScreen DRIVER_RIDE_RATING_SCREEN) "in_screen" "feedback_click"

data ScreenOutput
  = Close
  | SendFeedBack DriverRideRatingScreenState

data Action
  = BackPressed
  | AfterRender
  | PrimaryButtonActionController PrimaryButtonController.Action
  | FeedbackChanged String
  | FeedBackClick FeedbackSuggestions
  | Rating Int

eval :: Action -> DriverRideRatingScreenState -> Eval Action ScreenOutput DriverRideRatingScreenState
eval BackPressed state = exit $ Close

eval (PrimaryButtonActionController (PrimaryButtonController.OnClick)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  exit $ SendFeedBack state

eval (FeedbackChanged value) state = continue state { data { feedback = value } }

eval (Rating index) state = continue state { data { rating = index } }

eval (FeedBackClick feedBackOption) state = continue state { data { activeFeedBackOption = Just feedBackOption, selectedFeedbackOption = getSelectedFeedbackOption feedBackOption } }

eval _ state = continue state

getFeedBackString :: FeedbackSuggestions -> String
getFeedBackString feedBackOption = case feedBackOption of
  CUSTOMER_RUDE_BEHAVIOUR -> getString RUDE_BEHAVIOUR
  LONG_WAIT_TIME -> getString LONG_WAITING_TIME
  DIDNT_COME_TO_PICUP -> getString DIDNT_COME_TO_PICUP_LOCATION
  _ -> ""

getSelectedFeedbackOption :: FeedbackSuggestions -> String
getSelectedFeedbackOption feedBackOption = case feedBackOption of
  CUSTOMER_RUDE_BEHAVIOUR -> "Customer rude behaviour"
  LONG_WAIT_TIME -> "Long waiting time"
  DIDNT_COME_TO_PICUP -> "Didn't come to pickup"
  _ -> ""
