{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SplashScreen.Controller where

import Effect (Effect)
import JBridge (requestLocation, initiateLocationServiceClient)
import Prelude (class Applicative, Unit, bind, pure, unit, class Show)
import PrestoDOM (Eval, continue)
import Screens.Types ( SplashScreenState)
import Log (trackAppScreenRender, trackAppScreenEvent, trackAppActionClick)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (SplashScreenState)
import Effect (Effect)
import Screens(ScreenName(..), getScreen)
import JBridge (requestLocation)
instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = case action of 
      AfterRender -> trackAppScreenRender appId "screen" (getScreen SPLASH_SCREEN)
      NoAction -> trackAppScreenEvent appId (getScreen SPLASH_SCREEN) "in_screen" "no_action"
      CurrentLocation str1 str2 -> trackAppScreenEvent appId (getScreen SPLASH_SCREEN) "in_screen" "currrent_location"

data ScreenOutput = NoOutput
data Action = NoAction | CurrentLocation String String | AfterRender

eval :: Action -> SplashScreenState -> Eval Action Unit SplashScreenState
eval AfterRender state = continue state
eval _ state = continue state

getCordinateAndLocation :: forall t15 t8 t9. Applicative t15 => t8 -> t9 -> Effect (t15 Unit)
getCordinateAndLocation state push = do
                    _ <- requestLocation unit
                    _ <- initiateLocationServiceClient
                    pure (pure unit)
