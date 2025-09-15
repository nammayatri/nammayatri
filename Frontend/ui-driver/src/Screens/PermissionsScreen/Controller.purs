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
import JBridge (checkAndAskNotificationPermission, checkOverlayPermission, firebaseLogEvent, isBatteryPermissionEnabled, isNotificationPermissionEnabled, isOverlayPermissionEnabled, requestAutoStartPermission, requestBatteryPermission, requestLocation, getAndroidVersion, isLocationPermissionEnabled, openUrlInApp)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppBackPress, trackAppEndScreen, trackAppScreenEvent, trackAppScreenRender, trackAppTextInput)
import Prelude (class Show, bind, discard, not, pure, unit, when, ($), (==), void, show, (<>), (&&), (/=))
import PrestoDOM (Eval, update, continue, continueWithCmd, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.PermissionsScreen.ScreenData (Permissions(..))
import Screens.Types (PermissionsScreenState, NotificationBody)
import Helpers.Utils (isParentView, emitLogoutApp, contactSupportNumber)
import Data.Maybe (Maybe(..))
import Common.Types.App (LazyCheck(..))
import Screens.Types as ST
import Components.OptionsMenu as OptionsMenu
import Components.BottomDrawerList as BottomDrawerList
import Storage (getValueToLocalStore, KeyStore(..))

instance showAction :: Show Action where
  show (BackPressed) = "BackPressed"
  show (NoAction) = "NoAction"
  show (PrimaryButtonActionController var1) = "PrimaryButtonActionController_" <> show var1
  show (ItemClick _) = "ItemClick"
  show (UpdateNotificationPermissionState) = "UpdateNotificationPermissionState"
  show (UpdateOverlayPermissionState) = "UpdateOverlayPermissionState"
  show (NotificationPermissionCallBack _) = "NotificationPermissionCallBack"
  show (OverlayPermissionSwitchCallBack _) = "OverlayPermissionSwitchCallBack"
  show (BatteryUsagePermissionCallBack _) = "BatteryUsagePermissionCallBack"
  show (LocationPermissionCallBack _) = "LocationPermissionCallBack"
  show (UpdateLocationPermissionState) = "UpdateLocationPermissionState"
  show (UpdateBatteryPermissionState) = "UpdateBatteryPermissionState"
  show (AfterRender) = "AfterRender"
  show (UpdateAllChecks _) = "UpdateAllChecks"
  show (PopUpModalLogoutAction var1) = "PopUpModalLogoutAction_" <> show var1
  show (AppOnboardingNavBarAC var1) = "AppOnboardingNavBarAC_" <> show var1
  show (FcmNotificationAction _ _) = "FcmNotificationAction"
  show (OptionsMenuAction var1) = "OptionsMenuAction_" <> show var1
  show (BottomDrawerListAC var1) = "BottomDrawerListAC_" <> show var1
  show (WhatsAppClick) = "WhatsAppClick"
  show (CallButtonClick) = "CallButtonClick"

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
        NotificationPermissionCallBack str -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "location_permission_callback"
        OverlayPermissionSwitchCallBack str -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "overlay_permission_switch_callback"
        BatteryUsagePermissionCallBack str -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "battery_usage_permission_callback"
        LocationPermissionCallBack str -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "location_permission_callback"
        UpdateAllChecks updatedState -> trackAppScreenEvent appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "update_all_checks"
        NoAction -> trackAppScreenEvent appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "no_action"
        FcmNotificationAction _ _ -> trackAppScreenEvent appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "fcm_notification"
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
        OptionsMenuAction act -> case act of
            OptionsMenu.BackgroundClick -> trackAppScreenEvent appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "options_menu_background_click"
            OptionsMenu.ItemClick item -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "options_menu_item_click"
        BottomDrawerListAC act -> case act of
            BottomDrawerList.Dismiss -> trackAppScreenEvent appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "bottom_drawer_list_dismiss"
            BottomDrawerList.OnAnimationEnd -> trackAppScreenEvent appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "bottom_drawer_list_on_animation_end"
            BottomDrawerList.OnItemClick item -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "bottom_drawer_list_on_item_click"
        WhatsAppClick -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "whatsapp_click"
        CallButtonClick -> trackAppActionClick appId (getScreen NEED_ACCESS_SCREEN) "in_screen" "call_button_click"

data ScreenOutput =  GoBack | GoToHome | LogoutAccount | GoToRegisteration PermissionsScreenState | FcmNotification String NotificationBody | GoToFaqsScreen PermissionsScreenState | SelectLang PermissionsScreenState

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
            | UpdateLocationPermissionState
            | UpdateBatteryPermissionState
            | AfterRender
            | FcmNotificationAction String NotificationBody
            | UpdateAllChecks PermissionsScreenState
            | PopUpModalLogoutAction PopUpModal.Action
            | AppOnboardingNavBarAC AppOnboardingNavBar.Action
            | OptionsMenuAction OptionsMenu.Action
            | BottomDrawerListAC BottomDrawerList.Action
            | WhatsAppClick
            | CallButtonClick

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
eval (FcmNotificationAction notificationType notificationBody) state = exit $ FcmNotification notificationType notificationBody

eval BackPressed state = 
  if state.props.logoutModalView then continue state { props { logoutModalView = false}}
  else if state.props.menuOptions then continue state{props{menuOptions = false}} 
  else if state.props.contactSupportModal == ST.SHOW then continue state { props { contactSupportModal = ST.ANIMATING}}
  else exit $ GoToRegisteration state -- DECIDE FOR ENABLED DRIVER

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

eval (OptionsMenuAction OptionsMenu.BackgroundClick) state = continue state{props{menuOptions = false}}

eval (OptionsMenuAction (OptionsMenu.ItemClick item)) state = do
  let newState = state{props{menuOptions = false}}
  case item of
    "logout" -> continue newState { props { logoutModalView = true }}
    "contact_support" -> continue newState { props { contactSupportModal = ST.SHOW}}
    "change_language" -> exit $ SelectLang newState
    "faqs" -> exit $ GoToFaqsScreen newState
    _ -> continue newState

eval (BottomDrawerListAC BottomDrawerList.Dismiss) state = continue state { props { contactSupportModal = ST.HIDE}}

eval (BottomDrawerListAC BottomDrawerList.OnAnimationEnd) state = continue state { props { contactSupportModal = if state.props.contactSupportModal == ST.ANIMATING then ST.HIDE else state.props.contactSupportModal}}

eval (BottomDrawerListAC (BottomDrawerList.OnItemClick item)) state = do
  case item.identifier of
    "whatsapp" -> continueWithCmd state [pure WhatsAppClick]
    "call" -> continueWithCmd state [pure CallButtonClick]
    _ -> continue state

eval WhatsAppClick state = continueWithCmd state [do
  let supportPhone = state.data.cityConfig.registration.supportWAN
      phone = "%0APhone%20Number%3A%20"<> getValueToLocalStore MOBILE_NUMBER_KEY
      dlNumber = getValueToLocalStore ENTERED_DL
      rcNumber = getValueToLocalStore ENTERED_RC
      dl = if (dlNumber /= "__failed") then ("%0ADL%20Number%3A%20"<> dlNumber) else ""
      rc = if (rcNumber /= "__failed") then ("%0ARC%20Number%3A%20"<> rcNumber) else ""
  void $ openUrlInApp $ "https://wa.me/" <> supportPhone <> "?text=Hi%20Team%2C%0AI%20would%20require%20help%20in%20onboarding%20%0A%E0%A4%AE%E0%A5%81%E0%A4%9D%E0%A5%87%20%E0%A4%AA%E0%A4%82%E0%A4%9C%E0%A5%80%E0%A4%95%E0%A4%B0%E0%A4%A3%20%E0%A4%AE%E0%A5%87%E0%A4%82%20%E0%A4%B8%E0%A4%B9%E0%A4%BE%E0%A4%AF%E0%A4%A4%E0%A4%BE%20%E0%A4%95%E0%A5%80%20%E0%A4%86%E0%A4%B5%E0%A4%B6%E0%A5%8D%E0%A4%AF%E0%A4%95%E0%A4%A4%E0%A4%BE%20%E0%A4%B9%E0%A5%8B%E0%A4%97%E0%A5%80" <> phone <> dl <> rc
  pure NoAction
  ]

eval CallButtonClick state = do
  void $ pure $ unsafePerformEffect $ contactSupportNumber ""
  continue state

eval (AppOnboardingNavBarAC (AppOnboardingNavBar.Logout)) state = continue state {props{menuOptions = not state.props.menuOptions}}

eval (AppOnboardingNavBarAC AppOnboardingNavBar.PrefixImgOnClick) state = continueWithCmd state [ do pure $ BackPressed]

eval (PopUpModalLogoutAction (PopUpModal.OnButton2Click)) state =  
  continue $ (state {props {logoutModalView= false}})

eval (PopUpModalLogoutAction (PopUpModal.OnButton1Click)) state = do
    if isParentView FunctionCall
        then do
            void $ pure $ emitLogoutApp Nothing
            continue state
        else do
            exit LogoutAccount 

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
