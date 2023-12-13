{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.SplashScreen.Controller where

import Prelude (Unit, class Show, pure, unit, bind, ($), discard)
import PrestoDOM (Eval, continue)
import Screens.Types (SplashScreenState)
import Log (trackAppScreenRender, trackAppBackPress, trackAppEndScreen, trackAppActionClick, trackAppScreenEvent)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen SPLASH_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen SPLASH_SCREEN)
      trackAppEndScreen appId (getScreen SPLASH_SCREEN)
    TryAgain -> trackAppScreenEvent appId (getScreen SPLASH_SCREEN) "in_screen" "try_again"
    Dummy -> trackAppScreenEvent appId (getScreen SPLASH_SCREEN) "in_screen" "dummy"
    OnClick -> trackAppActionClick appId (getScreen SPLASH_SCREEN) "in_screen" "on_click"

data ScreenOutput
  = Retry
  | Cancel

data Action
  = BackPressed
  | TryAgain
  | Dummy
  | OnClick
  | AfterRender

eval :: Action -> SplashScreenState -> Eval Action Unit SplashScreenState
eval AfterRender state = continue state

eval BackPressed state = continue state

eval TryAgain state = continue state

eval Dummy state = continue state

eval (OnClick) state = continue state
