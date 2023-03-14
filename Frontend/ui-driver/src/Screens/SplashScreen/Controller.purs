module Screens.SplashScreen.Controller where

import Prelude (Unit, class Show, pure, unit, bind, ($), discard)
import PrestoDOM (Eval, continue)
import Screens.Types ( SplashScreenState)
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

data ScreenOutput = Retry | Cancel
data Action = BackPressed | TryAgain | Dummy | OnClick | AfterRender

eval :: Action -> SplashScreenState -> Eval Action Unit SplashScreenState
eval AfterRender state = continue state
eval BackPressed state = continue state
eval TryAgain state = continue state

eval Dummy state = continue state

eval (OnClick) state = continue state
