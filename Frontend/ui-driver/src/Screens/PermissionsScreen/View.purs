{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.PermissionsScreen.View where

import Common.Types.App
import Screens.PermissionsScreen.ComponentConfig

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.GenericHeader as GenericHeader
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import Debug (spy)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, map, pure, unit, ($), (&&), (<<<), (<>), (==), (>), not, void, discard)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, background, color, cornerRadius, fontStyle, frameLayout, gravity, height, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, scrollView, stroke, text, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.PermissionsScreen.Controller (Action(..), eval, ScreenOutput, getTitle, getDescription)
import Screens.PermissionsScreen.ScreenData (Listtype, Permissions(..), permissionsList)
import Screens.RegistrationScreen.ComponentConfig (logoutPopUp)
import Screens.Types as ST
import Styles.Colors as Color
import Web.HTML.History (back)
import Engineering.Helpers.Events as EHE
import Helpers.Utils as HU

screen :: ST.PermissionsScreenState -> Screen Action ST.PermissionsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "PermissionsScreen"
  , globalEvents : [ (\ push -> do
    void $ JB.storeCallBackBatteryUsagePermission push BatteryUsagePermissionCallBack
    void $ JB.storeCallBackNotificationPermission push NotificationPermissionCallBack
    void $ JB.storeCallBackOverlayPermission push OverlayPermissionSwitchCallBack
    void $ if initialState.data.config.permissions.locationPermission then JB.storeCallBackDriverLocationPermission push LocationPermissionCallBack else pure unit
    let _ = EHE.addEvent (EHE.defaultEventObject $ HU.getRegisterationStepScreenLoadedEventName ST.GRANT_PERMISSION)
    pure $ pure unit)]
  , eval:
      ( \state action -> do
          let _ = spy "PermissionsScreen ----- state" state
          let _ = spy "PermissionsScreen --------action" action
          eval state action
      ) 
  }

view :: forall w. (Action -> Effect Unit) -> ST.PermissionsScreenState -> PrestoDOM (Effect Unit) w
view push state = 
    linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , afterRender push $ const AfterRender
    , onBackPressed push $ const BackPressed
    , background Color.white900
    ] $
      [  linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , weight 1.0
        ][ PrestoAnim.animationSet[ Anim.fadeIn true] 
          $ headerView state push 
          , scrollView
            [ width MATCH_PARENT
            , weight 1.0
            , height MATCH_PARENT
            ][ linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ][ permissionsListView state push]
              ]
          ]
      ,  linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , visibility if state.props.logoutModalView then GONE else VISIBLE
          ][PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)]
    ]<> if state.props.logoutModalView then [logoutPopupModal push state] else []

headerView :: forall w. ST.PermissionsScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerView state push = AppOnboardingNavBar.view (push <<< AppOnboardingNavBarAC) (appOnboardingNavBarConfig state)


permissionsListView :: forall w. ST.PermissionsScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
permissionsListView state push = 
    linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , padding $ PaddingVertical 24 24
    ][linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        ] (map(\item ->
            let isPermissionEnabled = case item.permission of
                                            Notifications -> state.props.isNotificationPermissionChecked
                                            Overlay -> state.props.isOverlayPermissionChecked
                                            AutoStart -> state.props.isAutoStartPermissionChecked
                                            Battery -> state.props.isBatteryOptimizationChecked
                                            LocationPermission -> state.props.isLocationPermissionChecked
            in
            linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity RIGHT
            , orientation VERTICAL
            , margin $ Margin 15 16 15 0
            , onClick push $ const $ ItemClick item.permission
            ][  linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , cornerRadii $ Corners 8.0 true true isPermissionEnabled true
                , padding $ Padding 12 12 12 12
                , stroke $ "1," <> (if isPermissionEnabled  then  Color.green900 else Color.grey900)
                , background $ if isPermissionEnabled then  Color.green200 else Color.white900          
                , gravity CENTER_VERTICAL
                ][  titleImage item isPermissionEnabled state,
                    titleAndDescriptionList item,
                    checkBox item state
                ]
            , textView $
              [ padding $ Padding 16 8 16 8
              , text $ getString WATCH_VIDEO
              , cornerRadii $ Corners 16.0 false false true true
              , color Color.blue900
              , background Color.blue600
              , visibility GONE--if isPermissionEnabled then GONE else VISIBLE
              ] <> FontStyle.tags TypoGraphy
            ])(
                permissionsList state 
                <> 
                    if (state.props.androidVersion > 12 && state.data.config.permissions.notification)
                        then [{permission: Notifications , icon : ""}] 
                        else []
                ) )
    ]

titleAndDescriptionList :: forall w. Listtype -> PrestoDOM (Effect Unit) w
titleAndDescriptionList item = 
 linearLayout
    [ height WRAP_CONTENT
    , orientation VERTICAL
    , weight 1.0
    ][  textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ getTitle item.permission
        , color Color.black800
        , margin $ MarginBottom 6
        ] <> FontStyle.body1 TypoGraphy,
        textView $
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , text $ getDescription item.permission
        , color Color.black700
        , margin $ MarginRight 40
        ] <> FontStyle.body3 TypoGraphy
    ]

checkBox :: forall w. Listtype -> ST.PermissionsScreenState -> PrestoDOM (Effect Unit) w
checkBox item state = 
 let isPermissionEnabled = case item.permission of
            Notifications -> state.props.isNotificationPermissionChecked
            Overlay -> state.props.isOverlayPermissionChecked
            AutoStart -> state.props.isAutoStartPermissionChecked
            Battery -> state.props.isBatteryOptimizationChecked
            LocationPermission -> state.props.isLocationPermissionChecked
 in
    imageView
    [ width $ V 18
    , height $ V 18
    , imageWithFallback if isPermissionEnabled then fetchImage FF_ASSET "ny_ic_green_tick" else fetchImage FF_COMMON_ASSET "ny_ic_radio_unselected" 
    ]
      

titleImage :: forall w. Listtype -> Boolean -> ST.PermissionsScreenState -> PrestoDOM (Effect Unit) w
titleImage item isPermissionEnabled state = 
    linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity CENTER
        , background if isPermissionEnabled then Color.white900 else state.data.config.themeColors.onboardingStepImgBg
        , margin $ MarginRight 15
        , padding $ Padding 8 8 8 8
        , cornerRadius 25.0
        ][ imageView
            [ imageWithFallback $ fetchImage FF_ASSET $ case item.permission of
                Notifications -> "ny_ic_permission_notification"
                Overlay -> "ny_ic_permission_overlay"
                AutoStart -> "ny_ic_permission_autostart"
                Battery -> "ny_ic_permission_battery"
                LocationPermission -> "ny_ic_permission_location"
            , width $ V 24
            , height $ V 24
            ]
        ]
  
logoutPopupModal :: forall w . (Action -> Effect Unit) -> ST.PermissionsScreenState -> PrestoDOM (Effect Unit) w
logoutPopupModal push state =
    linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.blackLessTrans
    ][ PopUpModal.view (push <<< PopUpModalLogoutAction) (logoutPopUp Language) ] 
