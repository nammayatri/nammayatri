{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.WriteToUsScreen.Controller where

import Prelude (class Show, pure, unit, ($), discard)
import PrestoDOM (Eval, continue, exit)
import Screens.Types (WriteToUsScreenState)
import PrestoDOM.Types.Core (class Loggable)
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Screens.WriteToUsScreen.ScreenData (ListOptions(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen WRITE_TO_US_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen WRITE_TO_US_SCREEN)
      trackAppEndScreen appId (getScreen WRITE_TO_US_SCREEN)
    PrimaryEditTextActionController act -> case act of
      PrimaryEditText.TextChanged id value -> trackAppTextInput appId (getScreen WRITE_TO_US_SCREEN) "ride_feedback_text_changed" "primary_edit_text"
      PrimaryEditText.FocusChanged _ -> trackAppTextInput appId (getScreen WRITE_TO_US_SCREEN) "ride_feedback_text_focus_changed" "primary_edit_text"
    PrimaryButtonActionController primaryButtonState act -> case primaryButtonState.props.isThankYouScreen of
      true -> case act of
        PrimaryButton.OnClick -> do
          trackAppActionClick appId (getScreen WRITE_TO_US_SCREEN) "primary_button" "go_to_home_on_click"
          trackAppEndScreen appId (getScreen WRITE_TO_US_SCREEN)
        PrimaryButton.NoAction -> trackAppActionClick appId (getScreen WRITE_TO_US_SCREEN) "primary_button" "go_to_home_no_action"
      false -> case act of
        PrimaryButton.OnClick -> do
          trackAppActionClick appId (getScreen WRITE_TO_US_SCREEN) "primary_button" "submit_on_click"
          trackAppEndScreen appId (getScreen WRITE_TO_US_SCREEN)
        PrimaryButton.NoAction -> trackAppActionClick appId (getScreen WRITE_TO_US_SCREEN) "primary_button" "submit_no_action"
    NoAction -> trackAppScreenEvent appId (getScreen WRITE_TO_US_SCREEN) "in_screen" "no_action"

data ScreenOutput
  = GoBack
  | GoToHomeScreen

data Action
  = NoAction
  | PrimaryEditTextActionController PrimaryEditText.Action
  | PrimaryButtonActionController WriteToUsScreenState PrimaryButton.Action
  | BackPressed
  | AfterRender

eval :: Action -> WriteToUsScreenState -> Eval Action ScreenOutput WriteToUsScreenState
eval AfterRender state = continue state

eval BackPressed state = exit GoBack

eval (PrimaryButtonActionController primaryButtonState (PrimaryButton.OnClick)) state =
  if (state.props.isThankYouScreen) then
    exit GoToHomeScreen
  else
    continue $ state { props = state.props { isThankYouScreen = true } }

eval (PrimaryEditTextActionController (PrimaryEditText.TextChanged id value)) state = continue state

eval _ state = continue state

getTitle :: ListOptions -> String
getTitle listOptions = case listOptions of
  Subject -> (getString SUBJECT)
  YourEmaiId -> (getString YOUR_EMAIL_ID)
  DescribeYourIssue -> (getString DESCRIBE_YOUR_ISSUE)
