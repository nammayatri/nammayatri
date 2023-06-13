{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.ChooseLanguageScreen.Controller where

import Components.MenuButton.Controller (Action(..)) as MenuButtonController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (class Show, discard, pure, unit, ($))
import PrestoDOM (Eval, continue, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (ChooseLanguageScreenState)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen CHOOSE_LANGUAGE_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen CHOOSE_LANGUAGE_SCREEN)
    MenuButtonActionController (MenuButtonController.OnClick config) -> trackAppActionClick appId (getScreen CHOOSE_LANGUAGE_SCREEN) "menu_button_action" config.id
    PrimaryButtonActionController act -> case act of
      PrimaryButtonController.OnClick -> do
        trackAppActionClick appId (getScreen CHOOSE_LANGUAGE_SCREEN) "primary_button_action" "Continue"
        trackAppEndScreen appId (getScreen CHOOSE_LANGUAGE_SCREEN)
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen CHOOSE_LANGUAGE_SCREEN) "primary_button" "no_action"

data Action
  = PrimaryButtonActionController PrimaryButtonController.Action
  | MenuButtonActionController MenuButtonController.Action
  | BackPressed
  | AfterRender

data ScreenOutput
  = NextScreen String
  | Refresh ChooseLanguageScreenState

eval :: Action -> ChooseLanguageScreenState -> Eval Action ScreenOutput ChooseLanguageScreenState
eval (MenuButtonActionController (MenuButtonController.OnClick config)) state = exit $ Refresh state { props { selectedLanguage = config.id } }

eval (PrimaryButtonActionController PrimaryButtonController.OnClick) state = updateAndExit state { props { exitAnimation = true } } $ NextScreen state.props.selectedLanguage

eval (PrimaryButtonActionController PrimaryButtonController.NoAction) state = continue state

eval AfterRender state = continue state

eval BackPressed state = continue state
