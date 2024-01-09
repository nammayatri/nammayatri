{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module PopUpOverlay.Controller where

import Prelude (Unit, pure, unit, class Show, ($), bind, discard)

import Effect (Effect)
import PrestoDOM (Eval, Props, exit, continue)
import PrestoDOM.Types.Core (class Loggable)
import PopUpOverlay.Types
import JBridge (firebaseLogEvent)
import Log (trackAppActionClick, trackAppScreenRender, trackAppEndScreen)
import Screens (getScreen, ScreenName(..))
import Engineering.Helpers.LogEvent (logEvent)
import Effect.Unsafe (unsafePerformEffect)
import Components.PopUpModal as PopUpModal


instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen APP_UPDATE_POPUP_SCREEN)
    OnAccept -> do
        trackAppActionClick appId (getScreen APP_UPDATE_POPUP_SCREEN) "in_screen" "accept_update"
        -- trackAppEndScreen appId (getScreen APP_UPDATE_POPUP_SCREEN)
    OnCloseClick -> do
        trackAppActionClick appId (getScreen APP_UPDATE_POPUP_SCREEN) "in_screen" "decline_update"
        -- trackAppEndScreen appId (getScreen APP_UPDATE_POPUP_SCREEN)
    AppUpdatedModelAction action-> trackAppActionClick appId (getScreen APP_UPDATE_POPUP_SCREEN) "in_screen" "on_popupmodal_click"

data ScreenOutput = Decline | Accept 

data Action = OnCloseClick
            | OnAccept
            | AfterRender
            | AppUpdatedModelAction PopUpModal.Action

eval :: Action -> PopUpOverlayState -> Eval Action ScreenOutput PopUpOverlayState
eval OnCloseClick state = do
    exit Decline 
eval OnAccept state = do 
  let _ = unsafePerformEffect $ logEvent state.logField "ny_user_update_popup_click"
  exit Accept
eval (AppUpdatedModelAction (PopUpModal.OnButton1Click)) state = exit Decline
eval (AppUpdatedModelAction (PopUpModal.OnButton2Click)) state = exit Accept
eval AfterRender state = continue state
eval _ state = continue state

overrides :: String -> (Action -> Effect Unit) -> PopUpOverlayState -> Props (Effect Unit)
overrides _ push state = [] 
