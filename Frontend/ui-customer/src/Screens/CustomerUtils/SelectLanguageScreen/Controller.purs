{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SelectLanguageScreen.Controller where

import Components.GenericHeader.Controller (Action(..)) as GenericHeaderController
import Components.MenuButton.Controller (Action(..)) as MenuButtonController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Log (printLog)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (class Show, pure, unit, bind, discard, void, ($), (/=), (==))
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (SelectLanguageScreenState)
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalStore)
import Data.Array as DA
import Data.Maybe (Maybe (..))
import Helpers.Utils (isParentView, emitTerminateApp)
import Common.Types.App (LazyCheck(..))

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

eval AfterRender state = continue state {props {selectedLanguage = if DA.any (_ == getValueToLocalStore LANGUAGE_KEY) ["__failed" , "(null)"] then "EN_US" else getValueToLocalStore LANGUAGE_KEY}}

eval (PrimaryButtonActionController PrimaryButtonController.OnClick) state = updateAndExit state $ UpdateLanguage state

eval (GenericHeaderActionController (GenericHeaderController.PrefixImgOnClick )) state = continueWithCmd state [do pure BackPressed]

eval BackPressed state = 
  if isParentView FunctionCall 
    then do 
      void $ pure $ emitTerminateApp Nothing true
      continue state
    else exit $ GoToHomeScreen

eval _ state = continue state