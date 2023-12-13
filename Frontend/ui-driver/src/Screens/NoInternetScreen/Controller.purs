{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NoInternetScreen.Controller where

import Prelude (class Show, bind, pure, unit, (==))
import Screens.Types (NoInternetScreenState)
import PrestoDOM.Types.Core (class Loggable)
import PrestoDOM (Eval, continue, exit, continueWithCmd)
import Components.PrimaryButton.Controller as PrimaryButtonController
import JBridge (isInternetAvailable, requestLocation)
import Screens (ScreenName(..), getScreen)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen NO_INTERNET_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen NO_INTERNET_SCREEN)
    PrimaryButtonActionController triggertype act -> do
      case triggertype of
        "LOCATION_DISABLED" -> case act of
          PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen NO_INTERNET_SCREEN) "primary_button" "grantaccess_on_click"
          PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen NO_INTERNET_SCREEN) "primary_button" "grantaccess_no_action"
        "INTERNET_ACTION" -> case act of
          PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen NO_INTERNET_SCREEN) "primary_button" "tryagain_on_click"
          PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen NO_INTERNET_SCREEN) "primary_button" "tryagain_no_action"
        _ -> trackAppScreenEvent appId (getScreen NO_INTERNET_SCREEN) "primary_button" "no_action"
    LocationPermissionCallBack isLocationPermissionEnabled -> trackAppScreenEvent appId (getScreen NO_INTERNET_SCREEN) "in_screen" "location_permission_callback"
    InternetActionCallBack isInternetAvailable -> trackAppScreenEvent appId (getScreen NO_INTERNET_SCREEN) "in_screen" "internet_action_callback"
    Reload -> trackAppScreenEvent appId (getScreen NO_INTERNET_SCREEN) "in_screen" "reload"
    NoAction -> trackAppScreenEvent appId (getScreen NO_INTERNET_SCREEN) "in_screen" "no_action"

data Action
  = PrimaryButtonActionController String PrimaryButtonController.Action
  | NoAction
  | Reload
  | BackPressed
  | LocationPermissionCallBack Boolean
  | InternetActionCallBack String
  | AfterRender

data ScreenOutput
  = GoBack
  | Refresh
  | InternetCallBack NoInternetScreenState
  | LocationCallBack NoInternetScreenState

eval :: Action -> NoInternetScreenState -> Eval Action ScreenOutput NoInternetScreenState
eval BackPressed state = exit GoBack

eval (LocationPermissionCallBack isLocationPermissionEnabled) state = do
  if isLocationPermissionEnabled then
    exit (LocationCallBack state)
  else
    continue state

eval (InternetActionCallBack isInternetAvailable) state = do
  if (isInternetAvailable == "true") then do
    exit (InternetCallBack state)
  else
    continue state

eval (PrimaryButtonActionController triggertype PrimaryButtonController.OnClick) state =
  if (triggertype == "LOCATION_DISABLED") then
    continueWithCmd state
      [ do
          _ <- requestLocation unit
          pure NoAction
      ]
  else
    continueWithCmd state
      [ do
          internetCondition <- isInternetAvailable unit
          if internetCondition then pure Reload else pure NoAction
      ]

eval Reload state = exit Refresh

eval _ state = continue state
