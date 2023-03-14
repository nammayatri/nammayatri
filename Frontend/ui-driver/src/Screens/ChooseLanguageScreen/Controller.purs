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
import Debug.Trace (spy)

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
eval (MenuButtonAction (MenuButton.OnSelection btnState)) state = continue state { props { selectedLanguage = btnState.text.value }}
eval (PrimaryButtonActionController (PrimaryButton.OnClick)) state = do
  _ <- pure $ setValueToLocalStore LANGUAGE_KEY state.props.selectedLanguage
  exit (GoToEnterMobileScreen state)
eval _ state = continue state