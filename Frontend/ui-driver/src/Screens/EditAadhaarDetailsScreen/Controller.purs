module Screens.EditAadhaarDetailsScreen.Controller where

import Prelude (class Show, bind, unit, pure, discard)
import PrestoDOM (Eval, continue, exit)
import Screens.Types (EditAadhaarDetailsScreenState)
import PrestoDOM.Types.Core (class Loggable)
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.EditAadhaarDetailsScreen.ScreenData(ListOptions(..))
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

data ScreenOutput = GoBack

data Action = NoAction 
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
getTitleFromList listOptions =
  case listOptions of
    AADHAAR_NUMBER -> (getString AADHAR_NUMBER)
    IMAGE_FRONT_SIDE -> (getString FRONT_SIDE_IMAGE)
    IMAGE_BACK_SIDE -> (getString BACK_SIDE_IMAGE)
