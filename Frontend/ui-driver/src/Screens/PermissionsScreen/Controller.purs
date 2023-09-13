{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.PermissionsScreen.Controller where

import Prelude(($), pure, class Show, unit, (==), not, bind, discard)
import PrestoDOM (Eval, continue, continueWithCmd, exit)
import Components.PrimaryButton.Controller as PrimaryButtonController
import Screens.Types (PermissionsScreenState)
import PrestoDOM.Types.Core (class Loggable)
import JBridge (checkOverlayPermission, requestAutoStartPermission, requestLocation ,isLocationPermissionEnabled, isOverlayPermissionEnabled, requestBatteryPermission, isBatteryPermissionEnabled, firebaseLogEvent)
import Effect.Class (liftEffect)
import Screens.PermissionsScreen.ScreenData (Permissions(..))
import Language.Strings(getString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import Components.PopUpModal.Controller as PopUpModal
import Components.StepsHeaderModel.Controller as StepsHeaderModelController

instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = case action of
        AfterRender -> trackAppScreenRender appId "screen" (getScreen NEED_ACCESS_SCREEN)
        BackPressed -> do
            trackAppBackPress appId (getScreen NEED_ACCESS_SCREEN)
            trackAppEndScreen appId (getScreen NEED_ACCESS_SCREEN)
        PrimaryButtonActionController act -> case act of
            PrimaryButtonController.OnClick -> do
                trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "primary_button" "allow_access_on_click"
                trackAppEndScreen appId (getScreen NEED_ACCESS_SCREEN)
            PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "primary_button" "no_action"
        ItemClick str -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "item_type"
        UpdateLocationPermissionState -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "update_location_permission_state"
        UpdateOverlayPermissionState -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "update_overlay_permission_state"
        UpdateBatteryPermissionState -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "update_battery_permission_state"
        LocationPermissionCallBack str -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "location_permission_callback"
        OverlayPermissionSwitchCallBack str -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "overlay_permission_switch_callback"
        BatteryUsagePermissionCallBack str -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "battery_usage_permission_callback"
        UpdateAllChecks updatedState -> trackAppScreenEvent appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "update_all_checks"
        NoAction -> trackAppScreenEvent appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "no_action"
        PopUpModalLogoutAction act -> case act of
             PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "popup_modal" "on_goback"
             PopUpModal.Tipbtnclick _ _ -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "popup_modal" "tip_button_click"
             PopUpModal.DismissPopup -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "popup_modal" "dismiss_popup"
             PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "popup_modal" "call_support"
             PopUpModal.NoAction -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "popup_modal_action" "no_action"
             PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "popup_modal_action" "image"
             PopUpModal.ETextController act -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "popup_modal_action" "primary_edit_text"
             PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen NEED_ACCESS_SCREEN) "popup_modal_action" "countdown_updated"
        StepsHeaderModelAC act -> case act of
           StepsHeaderModelController.OnArrowClick -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "in_screen" "steps_header_on_click"
           StepsHeaderModelController.Logout -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "in_screen" "steps_header_logout"

data ScreenOutput =  GoBack | GoToHome | LogoutAccount

data Action = BackPressed
            | NoAction
            | PrimaryButtonActionController PrimaryButtonController.Action
            | ItemClick Permissions
            | UpdateLocationPermissionState
            | UpdateOverlayPermissionState
            | LocationPermissionCallBack String
            | OverlayPermissionSwitchCallBack String
            | BatteryUsagePermissionCallBack String
            | UpdateBatteryPermissionState
            | AfterRender
            | UpdateAllChecks PermissionsScreenState
            | StepsHeaderModelAC StepsHeaderModelController.Action
            | PopUpModalLogoutAction PopUpModal.Action

eval :: Action -> PermissionsScreenState -> Eval Action ScreenOutput PermissionsScreenState
eval AfterRender state = continueWithCmd state [ do 
                            isLocationPermission <- isLocationPermissionEnabled unit
                            isOverlayPermission <- isOverlayPermissionEnabled unit
                            isBatteryUsagePermission <- isBatteryPermissionEnabled unit
                            pure $ UpdateAllChecks state{ props { isLocationPermissionChecked = isLocationPermission,
                                                        isOverlayPermissionChecked = isOverlayPermission,
                                                        isBatteryOptimizationChecked = isBatteryUsagePermission}}]

eval (UpdateAllChecks updatedState) state = continue updatedState
eval BackPressed state = exit GoBack
eval (PrimaryButtonActionController (PrimaryButtonController.OnClick)) state = exit $ GoToHome 
eval UpdateLocationPermissionState state = continue state {props {isLocationPermissionChecked = true}}
eval UpdateOverlayPermissionState state = continue state {props {isOverlayPermissionChecked = true}}
eval UpdateBatteryPermissionState state = continue state {props {isBatteryOptimizationChecked = true}}

eval (LocationPermissionCallBack isLocationPermissionEnabled) state = do
  if isLocationPermissionEnabled == "true" then do
    _ <- pure $ firebaseLogEvent "permission_granted_location"
    continue state {props {isLocationPermissionChecked = true}}
    else continue state{props{isLocationPermissionChecked = false}}

eval (OverlayPermissionSwitchCallBack isOverlayPermissionEnabled) state = do
  if isOverlayPermissionEnabled == "true" then do
    _ <- pure $ firebaseLogEvent "permission_granted_overlay"
    continue state {props {isOverlayPermissionChecked = true }}
    else continue state{props{isOverlayPermissionChecked = false}}

eval (BatteryUsagePermissionCallBack isBatteryOptimizationEnabled) state = do
  if isBatteryOptimizationEnabled == "true" then do
    _ <- pure $ firebaseLogEvent "permission_granted_battery"
    continue state {props {isBatteryOptimizationChecked = true }}
    else continue state{props{isBatteryOptimizationChecked = false}}

eval (ItemClick itemType) state =
    case itemType of 
    Location -> do
        _ <- pure $ firebaseLogEvent "permission_btn_click_location"
        if not(state.props.isLocationPermissionChecked) then do
            continueWithCmd state [do
                isLocationPermission <- isLocationPermissionEnabled unit
                if (isLocationPermission) then pure UpdateLocationPermissionState
                    else do 
                    _ <- liftEffect $ requestLocation unit
                    pure NoAction
                ]
            else continue state

    Overlay -> do
        _ <- pure $ firebaseLogEvent "permission_btn_click_overlay"
        if not(state.props.isOverlayPermissionChecked) then do
            continueWithCmd state [do
                isOverlayPermission <- isOverlayPermissionEnabled unit
                if (isOverlayPermission) then pure UpdateOverlayPermissionState
                    else do 
                    _ <- liftEffect $ checkOverlayPermission unit
                    pure NoAction
                ]
            else continue state

    AutoStart -> do
        _ <- pure $ firebaseLogEvent "permission_btn_click_autostart"
        if not(state.props.isAutoStartPermissionChecked) then do
            continueWithCmd state {props {isAutoStartPermissionChecked = true}} [do
                _ <- liftEffect $ requestAutoStartPermission unit
                pure NoAction
            ]
            else continue state

    Battery -> do
        _ <- pure $ firebaseLogEvent "permission_btn_click_battery"
        if not(state.props.isBatteryOptimizationChecked) then do
            continueWithCmd state [do
                isBatteryUsagePermission <- isBatteryPermissionEnabled unit
                if (isBatteryUsagePermission) then pure UpdateBatteryPermissionState
                    else do 
                    _ <- liftEffect $ requestBatteryPermission unit
                    pure NoAction
                ]
            else continue state
eval (StepsHeaderModelAC (StepsHeaderModelController.Logout)) state = continue $ (state {props{logoutModalView = true}})

eval (StepsHeaderModelAC StepsHeaderModelController.OnArrowClick) state = continueWithCmd state [ do pure $ BackPressed]

eval (PopUpModalLogoutAction (PopUpModal.OnButton2Click)) state = continue $ (state {props {logoutModalView= false}})

eval (PopUpModalLogoutAction (PopUpModal.OnButton1Click)) state = exit $ LogoutAccount

eval _ state = continue state

getTitle :: Permissions -> String
getTitle permission = 
   case permission of
      Overlay -> (getString OVERLAY_TO_DRAW_OVER_APPLICATIONS)
      Battery -> (getString BATTERY_OPTIMIZATIONS)
      AutoStart -> (getString AUTO_START_APPLICATION_IN_BACKGROUND)
      Location -> (getString LOCATION_ACCESS)

getDescription :: Permissions -> String
getDescription permission = 
   case permission of
      Overlay ->  (getString NEED_IT_TO_SHOW_YOU_INCOMING_RIDE_REQUEST)
      Battery -> (getString NEED_IT_TO_DISABLE_BATTERY_OPTIMIZATION_FOR_THE_APP)
      AutoStart -> (getString NEED_IT_TO_AUTOSTART_YOUR_APP)
      Location -> ("Required to start receiving ride requests")--(getString NEED_IT_TO_ENABLE_LOCATION)
