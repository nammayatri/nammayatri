module Screens.AppUpdatePopUp.Controller where

import Prelude (Unit, pure, unit, class Show, ($), bind, discard)

import Effect (Effect)
import PrestoDOM (Eval, Props, exit, continue)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (AppUpdatePopUpState)
import JBridge (firebaseLogEvent)
import Log (trackAppActionClick, trackAppScreenRender, trackAppEndScreen)
import Screens (getScreen, ScreenName(..))

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen APP_UPDATE_POPUP_SCREEN)
    OnAccept -> do
        trackAppActionClick appId (getScreen APP_UPDATE_POPUP_SCREEN) "in_screen" "accept_update"
        trackAppEndScreen appId (getScreen APP_UPDATE_POPUP_SCREEN)
    OnCloseClick -> do
        trackAppActionClick appId (getScreen APP_UPDATE_POPUP_SCREEN) "in_screen" "decline_update"
        trackAppEndScreen appId (getScreen APP_UPDATE_POPUP_SCREEN)

data ScreenOutput = Decline | Accept 

data Action = OnCloseClick
            | OnAccept
            | AfterRender

eval :: Action -> AppUpdatePopUpState -> Eval Action ScreenOutput AppUpdatePopUpState
eval OnCloseClick state = do
    exit Decline 
eval OnAccept state = do 
  _ <- pure $ firebaseLogEvent "ny_user_update_popup_click"
  exit Accept

eval AfterRender state = continue state

overrides :: String -> (Action -> Effect Unit) -> AppUpdatePopUpState -> Props (Effect Unit)
overrides _ push state = [] 
