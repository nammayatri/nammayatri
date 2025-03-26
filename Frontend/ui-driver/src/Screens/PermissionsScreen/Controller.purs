{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.PermissionsScreen.Controller where

import Debug

import Components.PopUpModal as PopUpModal
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.GenericHeader.Controller as GenericHeader
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent)
import JBridge (checkAndAskNotificationPermission, checkOverlayPermission, firebaseLogEvent, isBatteryPermissionEnabled, isNotificationPermissionEnabled, isOverlayPermissionEnabled, requestAutoStartPermission, requestBatteryPermission, requestLocation, getAndroidVersion, isLocationPermissionEnabled)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppBackPress, trackAppEndScreen, trackAppScreenEvent, trackAppScreenRender, trackAppTextInput)
import Prelude (class Show, bind, discard, not, pure, unit, when, ($), (==), void)
import PrestoDOM (Eval, update, continue, continueWithCmd, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.PermissionsScreen.ScreenData (Permissions(..))
import Screens.Types (PermissionsScreenState)

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
        UpdateNotificationPermissionState -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "update_location_permission_state"
        UpdateOverlayPermissionState -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "update_overlay_permission_state"
        UpdateBatteryPermissionState -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "update_battery_permission_state"
        UpdateLocationPermissionState -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "location_permission_callback"
        UpdateCameraPermissionState -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "camera_permission_callback"
        NotificationPermissionCallBack str -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "location_permission_callback"
        OverlayPermissionSwitchCallBack str -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "overlay_permission_switch_callback"
        BatteryUsagePermissionCallBack str -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "battery_usage_permission_callback"
        LocationPermissionCallBack str -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "location_permission_callback"
        CameraPermissionCallBack str -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "camera_permission_callback"
        UpdateAllChecks updatedState -> trackAppScreenEvent appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "update_all_checks"
        NoAction -> trackAppScreenEvent appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "no_action"
        PopUpModalLogoutAction act -> case act of
            PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "popup_modal" "on_goback"
            PopUpModal.DismissPopup -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "popup_modal" "dismiss_popup"
            PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "popup_modal" "call_support"
            PopUpModal.NoAction -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "popup_modal_action" "no_action"
            PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "popup_modal_action" "image"
            PopUpModal.ETextController act -> trackAppTextInput appId (getScreen NEED_ACCESS_SCREEN) "popup_modal_action" "primary_edit_text"
            PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen NEED_ACCESS_SCREEN) "popup_modal_action" "countdown_updated"
            _ -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "popup_modal_action" "no_action"
        AppOnboardingNavBarAC act -> case act of
            AppOnboardingNavBar.GenericHeaderAC genericHeaderAction -> case genericHeaderAction of 
                GenericHeader.PrefixImgOnClick -> trackAppScreenEvent appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "generic_header_on_click"
                GenericHeader.SuffixImgOnClick -> trackAppScreenEvent appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "generic_header_on_click"
            AppOnboardingNavBar.Logout -> trackAppScreenEvent appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "onboarding_nav_bar_logout"
            AppOnboardingNavBar.PrefixImgOnClick -> trackAppScreenEvent appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "app_onboarding_nav_bar_prefix_img_on_click"

data ScreenOutput =  GoBack | GoToHome | LogoutAccount | GoToRegisteration PermissionsScreenState

data Action = BackPressed
            | NoAction
            | PrimaryButtonActionController PrimaryButtonController.Action
            | ItemClick Permissions
            | UpdateNotificationPermissionState
            | UpdateOverlayPermissionState
            | NotificationPermissionCallBack Boolean
            | OverlayPermissionSwitchCallBack Boolean
            | BatteryUsagePermissionCallBack Boolean
            | LocationPermissionCallBack Boolean
            | CameraPermissionCallBack Boolean
            | UpdateLocationPermissionState
            | UpdateCameraPermissionState
            | UpdateBatteryPermissionState
            | AfterRender
            | UpdateAllChecks PermissionsScreenState
            | PopUpModalLogoutAction PopUpModal.Action
            | AppOnboardingNavBarAC AppOnboardingNavBar.Action

eval :: Action -> PermissionsScreenState -> Eval Action ScreenOutput PermissionsScreenState
eval AfterRender state = continueWithCmd state [ do 
                            androidVersion <- getAndroidVersion
                            isNotificationPermissionChecked <- isNotificationPermissionEnabled unit
                            isOverlayPermission <- isOverlayPermissionEnabled unit
                            isBatteryUsagePermission <- isBatteryPermissionEnabled unit
                            isLocationPermission <- isLocationPermissionEnabled unit
                            pure $ UpdateAllChecks state{ props { isNotificationPermissionChecked = isNotificationPermissionChecked,
                                                        isOverlayPermissionChecked = isOverlayPermission,
                                                        isLocationPermissionChecked = isLocationPermission,
                                                        isBatteryOptimizationChecked = isBatteryUsagePermission, androidVersion = androidVersion}}]

eval (UpdateAllChecks updatedState) state = continue updatedState
eval BackPressed state = exit $ GoToRegisteration state -- DECIDE FOR ENABLED DRIVER
eval (PrimaryButtonActionController (PrimaryButtonController.OnClick)) state = exit $ if state.props.isDriverEnabled then GoToHome else GoToRegisteration state
eval UpdateNotificationPermissionState state = continue state {props {isNotificationPermissionChecked = true}}
eval UpdateOverlayPermissionState state = continue state {props {isOverlayPermissionChecked = true}}
eval UpdateBatteryPermissionState state = continue state {props {isBatteryOptimizationChecked = true}}
eval UpdateLocationPermissionState state = continue state {props {isLocationPermissionChecked = true}}

eval (NotificationPermissionCallBack isNotificationPermissionEnabled) state = do
  if isNotificationPermissionEnabled then do
    let _ = unsafePerformEffect $ logEvent state.data.logField  "permission_granted_notification"
    continue state {props {isNotificationPermissionChecked = isNotificationPermissionEnabled}}
    else continue state {props {isNotificationPermissionChecked = isNotificationPermissionEnabled}}

eval (OverlayPermissionSwitchCallBack isOverlayPermissionEnabled) state = do
  if isOverlayPermissionEnabled then do 
    let _ = unsafePerformEffect $ logEvent state.data.logField "permission_granted_overlay"
    continue state {props {isOverlayPermissionChecked = isOverlayPermissionEnabled}}
    else continue state {props {isOverlayPermissionChecked = isOverlayPermissionEnabled}}

eval (BatteryUsagePermissionCallBack isBatteryOptimizationEnabled) state = do
  if isBatteryOptimizationEnabled then do 
    let _ = unsafePerformEffect $ logEvent state.data.logField "permission_granted_battery"
    continue state {props {isBatteryOptimizationChecked = isBatteryOptimizationEnabled }}
    else continue state {props {isBatteryOptimizationChecked = isBatteryOptimizationEnabled }}

eval (LocationPermissionCallBack isLocationPermissionEnabled) state = do
    let newState = state {props {isLocationPermissionChecked = isLocationPermissionEnabled}}
    if isLocationPermissionEnabled then do
        let _ = unsafePerformEffect $ logEvent state.data.logField  "permission_granted_location"
        continue newState
    else continue newState

eval (ItemClick itemType) state =
    case itemType of 
    Notifications -> do        
        if not(state.props.isNotificationPermissionChecked) then do
            continueWithCmd state [do
                isNotificationPermission <- isNotificationPermissionEnabled unit
                if (isNotificationPermission) then pure UpdateNotificationPermissionState
                else do
                    _ <- checkAndAskNotificationPermission true
                    pure NoAction
                ]
            else continue state

    Overlay -> do
        let _ = unsafePerformEffect $ logEvent state.data.logField "permission_btn_click_overlay"
        if not(state.props.isOverlayPermissionChecked) then do
            continueWithCmd state [do
                isOverlayPermission <- isOverlayPermissionEnabled unit
                if isOverlayPermission then pure UpdateOverlayPermissionState
                    else do 
                    _ <- liftEffect $ checkOverlayPermission unit
                    pure NoAction
                ]
            else continue state

    LocationPermission -> do
        let _ = unsafePerformEffect $ logEvent state.data.logField "permission_btn_click_location"
        if not(state.props.isLocationPermissionChecked) then do
            continueWithCmd state [do
                isLocationPermission <- isLocationPermissionEnabled unit
                if isLocationPermission then pure UpdateLocationPermissionState
                    else do 
                        void $ liftEffect $ requestLocation unit
                        pure NoAction
                ]
            else continue state

    AutoStart -> do
        let _ = unsafePerformEffect $ logEvent state.data.logField "permission_btn_click_autostart"
        if not(state.props.isAutoStartPermissionChecked) then do
            continueWithCmd state {props {isAutoStartPermissionChecked = true}} [do
                _ <- liftEffect $ requestAutoStartPermission unit
                pure NoAction
            ]
            else continue state

    Battery -> do
        let _ = unsafePerformEffect $ logEvent state.data.logField "permission_btn_click_battery"
        if not(state.props.isBatteryOptimizationChecked) then do
            continueWithCmd state [do
                isBatteryUsagePermission <- isBatteryPermissionEnabled unit
                if (isBatteryUsagePermission) then pure UpdateBatteryPermissionState
                    else do 
                    _ <- liftEffect $ requestBatteryPermission unit
                    pure NoAction
                ]
            else continue state

eval (AppOnboardingNavBarAC (AppOnboardingNavBar.Logout)) state = continue $ (state {props{logoutModalView = true}})

eval (AppOnboardingNavBarAC AppOnboardingNavBar.PrefixImgOnClick) state = continueWithCmd state [ do pure $ BackPressed]

eval (PopUpModalLogoutAction (PopUpModal.OnButton2Click)) state = continue $ (state {props {logoutModalView= false}})

eval (PopUpModalLogoutAction (PopUpModal.OnButton1Click)) state = exit $ LogoutAccount

eval (PopUpModalLogoutAction (PopUpModal.DismissPopup)) state = continue state {props {logoutModalView= false}}

eval _ state = update state

getTitle :: Permissions -> String
getTitle permission = 
   case permission of
      Overlay -> (getString OVERLAY_TO_DRAW_OVER_APPLICATIONS)
      Battery -> (getString BATTERY_OPTIMIZATIONS)
      AutoStart -> (getString AUTO_START_APPLICATION_IN_BACKGROUND)
      Notifications -> (getString NOTIFICATION_ACCESS)
      LocationPermission -> getString LOCATION_ACCESS

getDescription :: Permissions -> String
getDescription permission = 
   case permission of
      Overlay -> (getString NEED_IT_TO_SHOW_YOU_INCOMING_RIDE_REQUEST)
      Battery -> (getString NEED_IT_TO_DISABLE_BATTERY_OPTIMIZATION_FOR_THE_APP)
      AutoStart -> (getString NEED_IT_TO_AUTOSTART_YOUR_APP)
      Notifications -> (getString NOTIFICATION_ACCESS_DESC)
      LocationPermission -> getString $ NEED_IT_TO_ENABLE_LOCATION "NEED_IT_TO_ENABLE_LOCATION" 
