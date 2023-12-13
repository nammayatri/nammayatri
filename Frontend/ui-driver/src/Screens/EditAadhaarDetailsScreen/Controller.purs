{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.EditAadhaarDetailsScreen.Controller where

import Prelude (class Show, bind, unit, pure, discard)
import PrestoDOM (Eval, continue, exit)
import Screens.Types (EditAadhaarDetailsScreenState)
import PrestoDOM.Types.Core (class Loggable)
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.EditAadhaarDetailsScreen.ScreenData (ListOptions(..))
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen EDIT_AADHAR_DETAILS_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen EDIT_AADHAR_DETAILS_SCREEN)
      trackAppEndScreen appId (getScreen EDIT_AADHAR_DETAILS_SCREEN)
    NoAction -> trackAppScreenEvent appId (getScreen EDIT_AADHAR_DETAILS_SCREEN) "in_screen" "no_action"
    PrimaryEditTextActionController act -> trackAppActionClick appId (getScreen EDIT_AADHAR_DETAILS_SCREEN) "in_screen" "primary_edit_text"
    PrimaryButtonActionController act -> trackAppActionClick appId (getScreen EDIT_AADHAR_DETAILS_SCREEN) "in_screen" "primary_button"
    ToggleScreenMode -> trackAppActionClick appId (getScreen EDIT_AADHAR_DETAILS_SCREEN) "in_screen" "toggle_screen_mode_on_click"

data ScreenOutput
  = GoBack

data Action
  = NoAction
  | PrimaryEditTextActionController PrimaryEditTextController.Action
  | PrimaryButtonActionController PrimaryButtonController.Action
  | BackPressed
  | ToggleScreenMode
  | AfterRender

eval :: Action -> EditAadhaarDetailsScreenState -> Eval Action ScreenOutput EditAadhaarDetailsScreenState
eval AfterRender state = continue state

eval BackPressed state = exit GoBack

eval _ state = continue state

getTitleFromList :: ListOptions -> String
getTitleFromList listOptions = case listOptions of
  AADHAAR_NUMBER -> (getString AADHAR_NUMBER)
  IMAGE_FRONT_SIDE -> (getString FRONT_SIDE_IMAGE)
  IMAGE_BACK_SIDE -> (getString BACK_SIDE_IMAGE)
