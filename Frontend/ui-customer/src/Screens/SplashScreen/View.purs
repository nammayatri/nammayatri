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