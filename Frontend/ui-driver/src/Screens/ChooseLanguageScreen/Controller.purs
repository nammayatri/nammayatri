{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ChooseLanguageScreen.Controller where

import Components.SelectMenuButton.Controller (Action(..)) as MenuButton
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress)
import Prelude (class Show, bind, discard, pure, ($), unit)
import PrestoDOM (Eval, continue, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (ChooseLanguageScreenState)
import Components.PrimaryButton.Controller as PrimaryButton
import Storage (KeyStore(..), setValueToLocalStore)
import JBridge(minimizeApp)
import Screens (ScreenName(..), getScreen)
import JBridge (setCleverTapUserProp)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent, logEventWithParams)
import Foreign (unsafeToForeign)
import Locale.Utils

instance showAction :: Show Action where
  show _ = ""

-- please use ScreenNames.purs file to add and use the screen names. -
-- Why we need the ScreenNames.purs? Consistancy for the screen names remain intact.
-- We can use the same string even in screen function in view file and while creating a push instance in handler file

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen CHOOSE_LANGUAGE_SCREEN)
    BackPressed -> trackAppBackPress appId (getScreen CHOOSE_LANGUAGE_SCREEN)
    MenuButtonAction (MenuButton.OnSelection btnState) -> trackAppActionClick appId (getScreen CHOOSE_LANGUAGE_SCREEN) "menu_button" btnState.text.value
    PrimaryButtonActionController act -> case act of
      PrimaryButton.OnClick -> do
        trackAppActionClick appId (getScreen CHOOSE_LANGUAGE_SCREEN) "primary_button_action" "next_on_click"
        trackAppEndScreen appId (getScreen CHOOSE_LANGUAGE_SCREEN)
      PrimaryButton.NoAction -> trackAppActionClick appId (getScreen CHOOSE_LANGUAGE_SCREEN) "primary_button_action" "no_action"

data ScreenOutput = GoToEnterMobileScreen ChooseLanguageScreenState

data Action = BackPressed | MenuButtonAction MenuButton.Action | PrimaryButtonActionController PrimaryButton.Action | AfterRender
eval :: Action -> ChooseLanguageScreenState -> Eval Action ScreenOutput ChooseLanguageScreenState
eval BackPressed state = do
  _ <- pure $ minimizeApp ""
  continue state
eval AfterRender state = continue state
eval (MenuButtonAction (MenuButton.OnSelection btnState)) state = do
  let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_choose_language"
  continue state { props { selectedLanguage = btnState.text.value }}
eval (PrimaryButtonActionController (PrimaryButton.OnClick)) state = do
  let _ = unsafePerformEffect $ logEventWithParams state.data.logField "ny_driver_choose_language_confirmation" "Language" state.props.selectedLanguage
      _ = setLanguageLocale state.props.selectedLanguage
  _ <- pure $ setCleverTapUserProp [{key : "Preferred Language", value : unsafeToForeign state.props.selectedLanguage}]
  exit (GoToEnterMobileScreen state)
eval _ state = continue state