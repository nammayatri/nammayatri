{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AboutUsScreen.Controller where

import Prelude (class Show, pure, unit, bind, void, ($), discard)
import Screens.Types (AboutUsScreenState)
import PrestoDOM (Eval, continue, exit, continueWithCmd)
import PrestoDOM.Types.Core (class Loggable)
import Components.GenericHeader.Controller (Action(..)) as GenericHeaderController
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import Helpers.Utils (emitTerminateApp, isParentView)
import Common.Types.App (LazyCheck (..))
import Data.Maybe (Maybe(..))

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen ABOUT_US_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen ABOUT_US_SCREEN)
      trackAppEndScreen appId (getScreen ABOUT_US_SCREEN)
    GenericHeaderActionController act -> case act of
      GenericHeaderController.PrefixImgOnClick -> do
        trackAppActionClick appId (getScreen ABOUT_US_SCREEN) "generic_header_action" "back_icon"
        trackAppEndScreen appId (getScreen ABOUT_US_SCREEN)
      GenericHeaderController.SuffixImgOnClick -> trackAppActionClick appId (getScreen ABOUT_US_SCREEN) "generic_header_action" "forward_icon"
    TermsAndConditions -> trackAppActionClick appId (getScreen ABOUT_US_SCREEN) "in_screen" "t_&_c"
    PrivacyPolicy -> trackAppActionClick appId (getScreen ABOUT_US_SCREEN) "in_screen" "privacy_policy"

data Action = GenericHeaderActionController GenericHeaderController.Action
            | BackPressed
            | TermsAndConditions
            | AfterRender
            | PrivacyPolicy

data ScreenOutput = GoToHomeScreen
eval :: Action -> AboutUsScreenState -> Eval Action ScreenOutput AboutUsScreenState

eval (GenericHeaderActionController (GenericHeaderController.PrefixImgOnClick)) state = continueWithCmd state [do pure BackPressed]

eval BackPressed state = 
  if isParentView FunctionCall 
    then do 
      void $ pure $ emitTerminateApp Nothing true
      continue state
    else exit $ GoToHomeScreen

eval _ state = continue state