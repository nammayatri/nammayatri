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
   _ <- pure $ setValueToLocalStore LANGUAGE_KEY state.props.selectedLanguage
   exit GoBack
eval BackPressed state = exit GoBack
eval AfterRender state = continue state {props {selectedLanguage = if getValueToLocalStore LANGUAGE_KEY == "__failed" then "EN_US" else getValueToLocalStore LANGUAGE_KEY}}
eval (MenuButtonAction (MenuButton.OnSelection btnState)) state = continue state { props { selectedLanguage = btnState.text.value }}
eval (PrimaryButtonActionController (PrimaryButtonController.NoAction)) state = continue state
