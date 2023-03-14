module Screens.RegistrationScreen.Controller where

import Prelude(class Show, unit, pure, unit, discard)
import PrestoDOM (Eval, continue, exit)
import Screens.Types (RegistrationScreenState)
import PrestoDOM.Types.Core (class Loggable)
import Components.PrimaryButton as PrimaryButtonController
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
   performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen REGISTRATION_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen REGISTRATION_SCREEN)
      trackAppEndScreen appId (getScreen REGISTRATION_SCREEN)
    PrimaryButtonAction act -> case act of 
      PrimaryButtonController.OnClick -> do
        trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "primary_button" "next_on_click"
        trackAppEndScreen appId (getScreen REGISTRATION_SCREEN)
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "primary_button" "no_action"
    NoAction -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "in_screen" "no_action"
    
data ScreenOutput = GoBack | GoToUploadDriverLicense
data Action = BackPressed 
            | NoAction
            | AfterRender
            | PrimaryButtonAction PrimaryButtonController.Action

eval :: Action -> RegistrationScreenState -> Eval Action ScreenOutput RegistrationScreenState
eval AfterRender state = continue state
eval BackPressed state = continue state
eval (PrimaryButtonAction (PrimaryButtonController.OnClick)) state = exit (GoToUploadDriverLicense)
eval _ state = continue state


