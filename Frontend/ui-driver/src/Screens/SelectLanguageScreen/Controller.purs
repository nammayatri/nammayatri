{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SelectLanguageScreen.Controller where

import Prelude (class Show, pure, unit, (==), bind, ($), discard)
import PrestoDOM (Eval, continue, exit)
import Screens.Types (SelectLanguageScreenState)
import PrestoDOM.Types.Core (class Loggable)
import Components.SelectMenuButton as MenuButton
import Components.PrimaryButton.Controller as PrimaryButtonController
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalStore)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress)
import Screens (ScreenName(..), getScreen)
import JBridge (setCleverTapUserProp)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent, logEventWithParams)
import Foreign (unsafeToForeign)
import Locale.Utils

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen SELECT_LANGUAGE_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen SELECT_LANGUAGE_SCREEN)
      trackAppEndScreen appId (getScreen SELECT_LANGUAGE_SCREEN)
    MenuButtonAction (MenuButton.OnSelection btnState)-> trackAppActionClick appId (getScreen SELECT_LANGUAGE_SCREEN) "menu_button" btnState.text.value
    PrimaryButtonActionController act -> case act of
      PrimaryButtonController.OnClick -> do
        trackAppActionClick appId (getScreen SELECT_LANGUAGE_SCREEN) "primary_button" "update_on_click"
        trackAppEndScreen appId (getScreen SELECT_LANGUAGE_SCREEN)
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen SELECT_LANGUAGE_SCREEN) "primary_button" "no_action"
      
data ScreenOutput = GoBack
data Action = BackPressed | MenuButtonAction MenuButton.Action | PrimaryButtonActionController PrimaryButtonController.Action | AfterRender   
eval :: Action -> SelectLanguageScreenState -> Eval Action ScreenOutput SelectLanguageScreenState
eval (PrimaryButtonActionController (PrimaryButtonController.OnClick)) state = do
  let _ = unsafePerformEffect $ logEventWithParams state.data.logField "ny_driver_language_selection" "Language" state.props.selectedLanguage
      _ = setLanguageLocale state.props.selectedLanguage
  _ <- pure $ setCleverTapUserProp [{key : "Preferred Language", value : unsafeToForeign state.props.selectedLanguage}]
  exit GoBack
eval BackPressed state = exit GoBack
eval AfterRender state = continue state {props {selectedLanguage = if getLanguageLocale languageKey == "__failed" then "EN_US" else getLanguageLocale languageKey}}
eval (MenuButtonAction (MenuButton.OnSelection btnState)) state = continue state { props { selectedLanguage = btnState.text.value }}
eval (PrimaryButtonActionController (PrimaryButtonController.NoAction)) state = continue state
