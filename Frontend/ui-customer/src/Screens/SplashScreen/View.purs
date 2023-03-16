{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SplashScreen.View where

import Prelude (Unit, bind, pure, unit, const)
import PrestoDOM (PrestoDOM, Screen, linearLayout, afterRender)
import Effect (Effect)
import Screens.SplashScreen.Controller (Action(..), eval, getCordinateAndLocation)
import Screens.Types (SplashScreenState)
import Debug.Trace (spy)
import Prelude (($))

screen :: SplashScreenState -> Screen Action SplashScreenState Unit
screen initialState =
  { initialState
  , view
  , name : "SplashScreen" -- _ <- getCurrentPosition push CurrentLocation
  , globalEvents : [getCordinateAndLocation initialState]
  , eval
  }
view
  :: forall w
  . (Action -> Effect Unit)
  -> SplashScreenState
  -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout 
  [ afterRender push  (const AfterRender)
  ][]