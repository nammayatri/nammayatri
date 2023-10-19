{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AppUpdatePopUpScreen.Controller where

import Prelude (Unit, pure, unit, class Show, bind)

import Effect (Effect)
import PrestoDOM (Eval, Props, exit, continue, continueWithCmd)
import Prelude (($))
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (AppUpdatePopUpScreenState)
import Components.PopUpModal as PopUpModal
import Screens (ScreenName(..), getScreen)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress)
import Components.PrimaryButton.Controller as PrimaryButtonController
import JBridge as JB
import Components.PrimaryButton as PrimaryButton
import Storage (KeyStore(..), setValueToLocalStore)
import Effect.Uncurried (runEffectFn1)

data ScreenOutput = Decline | Accept | Exit

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen APP_UPDATE_POPUP_SCREEN)
    OnCloseClick -> trackAppActionClick appId (getScreen APP_UPDATE_POPUP_SCREEN) "in_screen" "on_close_click"
    OnAccept -> trackAppActionClick appId (getScreen APP_UPDATE_POPUP_SCREEN) "in_screen" "on_accept_click"
    BackPressed -> trackAppActionClick appId (getScreen APP_UPDATE_POPUP_SCREEN) "in_screen" "on_accept_click"
    OnResumeCallBack -> trackAppActionClick appId (getScreen APP_UPDATE_POPUP_SCREEN) "in_screen" "on_accept_click"
    PrimaryButtonActionController action-> trackAppActionClick appId (getScreen APP_UPDATE_POPUP_SCREEN) "in_screen" "on_accept_click"
    AppUpdatedModelAction action-> trackAppActionClick appId (getScreen APP_UPDATE_POPUP_SCREEN) "in_screen" "on_popupmodal_click"
    _ -> pure unit

data Action = OnCloseClick
            | OnAccept
            | AfterRender
            | BackPressed
            | OnResumeCallBack
            | PrimaryButtonActionController PrimaryButtonController.Action
            | AppUpdatedModelAction PopUpModal.Action
            | NoAction
            | ExitScreen


eval :: Action -> AppUpdatePopUpScreenState -> Eval Action ScreenOutput AppUpdatePopUpScreenState
eval OnCloseClick state = do
    exit Decline 
eval OnAccept state = do 
    exit Accept 
eval (PrimaryButtonActionController (PrimaryButton.OnClick)) state = do
  continueWithCmd state [do
    isEnabled <- runEffectFn1 JB.isNetworkTimeEnabled unit
    if isEnabled then do
      pure OnResumeCallBack
    else do
      _ <- pure $ JB.launchDateSettings ""
      pure NoAction
    ]


eval BackPressed state = do 
  _ <- pure $ JB.minimizeApp ""
  continue state
eval OnResumeCallBack state = do
  continueWithCmd state [do
    isEnabled <- runEffectFn1 JB.isNetworkTimeEnabled unit
    if isEnabled then do
      pure ExitScreen
    else pure NoAction
    ]
eval (AppUpdatedModelAction (PopUpModal.PrimaryButton1 PrimaryButtonController.OnClick)) state = exit Decline
eval (AppUpdatedModelAction (PopUpModal.PrimaryButton2 PrimaryButtonController.OnClick)) state = exit Accept
eval ExitScreen state = exit Exit
eval _ state = continue state

overrides :: String -> (Action -> Effect Unit) -> AppUpdatePopUpScreenState -> Props (Effect Unit)
overrides _ push state = [] 
