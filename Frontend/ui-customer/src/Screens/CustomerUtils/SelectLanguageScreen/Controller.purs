module Screens.SelectLanguageScreen.Controller where

import Components.GenericHeader.Controller (Action(..)) as GenericHeaderController
import Components.MenuButton.Controller (Action(..)) as MenuButtonController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Log (printLog)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (class Show, pure, unit, bind, discard, ($), (/=), discard, (==))
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (SelectLanguageScreenState)
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalStore)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen SELECT_LANGUAGE_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen SELECT_LANGUAGE_SCREEN)
      trackAppEndScreen appId (getScreen SELECT_LANGUAGE_SCREEN)
    PrimaryButtonActionController act -> case act of 
      PrimaryButtonController.OnClick -> do
        trackAppActionClick appId (getScreen SELECT_LANGUAGE_SCREEN) "primary_button" "update"
        trackAppEndScreen appId (getScreen SELECT_LANGUAGE_SCREEN)
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen SELECT_LANGUAGE_SCREEN) "primary_button" "no_action"
    MenuButtonActionController (MenuButtonController.OnClick config) -> trackAppActionClick appId (getScreen SELECT_LANGUAGE_SCREEN) "menu_button" config.id
    GenericHeaderActionController act -> case act of
      GenericHeaderController.PrefixImgOnClick -> do
        trackAppActionClick appId (getScreen SELECT_LANGUAGE_SCREEN) "generic_header_action" "back_icon"
        trackAppEndScreen appId (getScreen SELECT_LANGUAGE_SCREEN)
      GenericHeaderController.SuffixImgOnClick -> trackAppActionClick appId (getScreen SELECT_LANGUAGE_SCREEN) "generic_header_action" "forward_icon"

data Action = PrimaryButtonActionController PrimaryButtonController.Action 
            | MenuButtonActionController MenuButtonController.Action
            | GenericHeaderActionController GenericHeaderController.Action
            | BackPressed
            | AfterRender

data ScreenOutput = UpdateLanguage SelectLanguageScreenState 
                  | GoToHomeScreen
eval :: Action -> SelectLanguageScreenState -> Eval Action ScreenOutput SelectLanguageScreenState

eval (MenuButtonActionController (MenuButtonController.OnClick config)) state = do
  let language = (getValueToLocalStore LANGUAGE_KEY)
  _ <- pure $ printLog "SelectLanguage Screen" language
  let isBtnActive = if config.id /= language then true else false
  continue state{props{selectedLanguage = config.id,btnActive = isBtnActive}}

eval AfterRender state = continue state {props {selectedLanguage = if getValueToLocalStore LANGUAGE_KEY == "__failed" then "EN_US" else getValueToLocalStore LANGUAGE_KEY}}

eval (PrimaryButtonActionController PrimaryButtonController.OnClick) state = updateAndExit state $ UpdateLanguage state

eval (GenericHeaderActionController (GenericHeaderController.PrefixImgOnClick )) state = continueWithCmd state [do pure BackPressed]

eval BackPressed state = exit $ GoToHomeScreen

eval _ state = continue state