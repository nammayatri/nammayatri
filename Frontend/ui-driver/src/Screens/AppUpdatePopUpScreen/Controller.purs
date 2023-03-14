module Screens.AppUpdatePopUpScreen.Controller where

import Prelude (Unit, pure, unit, class Show)

import Effect (Effect)
import PrestoDOM (Eval, Props, exit, continue)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (AppUpdatePopUpScreenState)
import Screens (ScreenName(..), getScreen)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress)

data ScreenOutput = Decline | Accept 

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen APP_UPDATE_POPUP_SCREEN)
    OnCloseClick -> trackAppActionClick appId (getScreen APP_UPDATE_POPUP_SCREEN) "in_screen" "on_close_click"
    OnAccept -> trackAppActionClick appId (getScreen APP_UPDATE_POPUP_SCREEN) "in_screen" "on_accept_click"
    
data Action = OnCloseClick
            | OnAccept
            | AfterRender

eval :: Action -> AppUpdatePopUpScreenState -> Eval Action ScreenOutput AppUpdatePopUpScreenState
eval OnCloseClick state = do
    exit Decline 
eval OnAccept state = do 
    exit Accept 

eval _ state = continue state

overrides :: String -> (Action -> Effect Unit) -> AppUpdatePopUpScreenState -> Props (Effect Unit)
overrides _ push state = [] 
