{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RegistrationScreen.Controller where

import Prelude(class Show, unit, pure, unit, discard)
import PrestoDOM (Eval, continue, exit)
import Screens.Types (RegistrationScreenState)
import PrestoDOM.Types.Core (class Loggable)
import Components.PrimaryButton as PrimaryButtonController
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import Screens.RegistrationScreen.ScreenData (ListOptions(..))
import Components.StepsHeaderModel.Controller as StepsHeaderModelController

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
    StepsHeaderModelAC act -> case act of
      StepsHeaderModelController.OnArrowClick -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "in_screen" "steps_header_on_click"
      StepsHeaderModelController.Logout -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "in_screen" "steps_header_logout"
    RegistrationAction value -> trackAppScreenRender appId "screen" (getScreen REGISTRATION_SCREEN)

    
data ScreenOutput = GoBack | GoToUploadDriverLicense
data Action = BackPressed 
            | NoAction
            | AfterRender
            | PrimaryButtonAction PrimaryButtonController.Action
            | StepsHeaderModelAC StepsHeaderModelController.Action
            | RegistrationAction ListOptions

eval :: Action -> RegistrationScreenState -> Eval Action ScreenOutput RegistrationScreenState
eval AfterRender state = continue state
eval BackPressed state = continue state
eval (PrimaryButtonAction (PrimaryButtonController.OnClick)) state = exit (GoToUploadDriverLicense)
eval (RegistrationAction item ) state = 
       case item of 
          DRIVING_LICENSE_OPTION -> exit (GoToUploadDriverLicense)
          VEHICLE_DETAILS_OPTION -> continue state
          GRANT_PERMISSION -> continue state

eval _ state = continue state


