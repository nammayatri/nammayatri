module Screens.AskPermissionScreen.View where

import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, ScopedScreen, alignParentBottom, background, clickable, color, gravity, height, imageView, imageWithFallback, linearLayout, margin, onBackPressed, orientation, padding, relativeLayout, text, textView, visibility, width)
import Screens.Types as ST
import Styles.Colors as Color
import Effect (Effect)
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.PrimaryButton as PrimaryButton
import Prelude (Unit, const, ($), (<<<), (<>), discard, void, pure, unit, (>), (<=))
import Screens.AskPermissionScreen.Controller (Action(..), eval, ScreenOutput)
import Screens.AskPermissionScreen.ComponentConfig
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Language.Strings (getString)
import Language.Types (STR(..))
import Data.Maybe (Maybe(..))
import Data.Array as DA
import Mobility.Prelude (boolToVisibility)
import JBridge as JB
import Components.StepsHeaderModel as SHM
import Debug (spy)

screen :: ST.AskPermissionScreenState -> ScopedScreen Action ST.AskPermissionScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "PermissionScreenV2"
  , parent: Just "PermissionScreenV2"
  , globalEvents:
      [ ( \push -> do
            void $ JB.storeCallBackBatteryUsagePermission push BatteryPermissionCallBack
            void $ JB.storeCallBackOverlayPermission push OverlayCallBack
            void $ JB.storeCallBackDriverLocationPermission push LocationCallBack
            pure $ pure unit
        )
      ]
  , eval:
      \action state -> do
        let
          _ = spy "AskPermissionScreen action " action
        let
          _ = spy "AskPermissionScreen state " state
        eval action state
  }

view :: forall w. (Action -> Effect Unit) -> ST.AskPermissionScreenState -> PrestoDOM (Effect Unit) w
view push state = if state.props.ifAnim then screenWithAnim else justScreen
  where
  screenWithAnim = Anim.translateYScreenAnimation $ justScreen
  justScreen =
    relativeLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
      , onBackPressed push $ const BackPressed
      , clickable true
      ]
      [ linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , gravity CENTER_HORIZONTAL
          , orientation VERTICAL
          ]
          [ linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , background Color.black900
              , orientation VERTICAL
              , padding $ PaddingVertical 20 20
              , visibility $ boolToVisibility $ DA.length state.data.permissionList > 1
              ]
              [ SHM.view (push <<< StepsHeaderModelAC) $ (stepsHeaderData state) ]
          , headerView push state
          , imageView
              [ imageWithFallback $ getIcon state
              , height $ V 132
              , width $ V 132
              , gravity CENTER
              , margin $ MarginTop 22
              ]
          , textView
              $ [ text $ getTitle state
                , color Color.black800
                , margin $ Margin 22 16 22 0
                , gravity CENTER
                ]
              <> FontStyle.h2 LanguageStyle
          , textView
              $ [ text $ getDesc state
                , color Color.black800
                , gravity CENTER
                , margin $ Margin 22 10 22 0
                ]
              <> FontStyle.subHeading2 TypoGraphy
          ]
      , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , alignParentBottom "true,-1"
          ]
          [ PrimaryButton.view (push <<< (PrimaryButtonActionController)) (primaryButtonConfig state) ]
      ]

headerView :: forall w. (Action -> Effect Unit) -> ST.AskPermissionScreenState -> PrestoDOM (Effect Unit) w
headerView push state =
  textView
    $ [ text $ getString GRANT_PERMISSIONS
      , color Color.white900
      , height WRAP_CONTENT
      , width MATCH_PARENT
      , background Color.black900
      , padding $ Padding 16 46 16 26
      , visibility $ boolToVisibility $ DA.length state.data.permissionList <= 1
      ]
    <> FontStyle.h1 TypoGraphy

getTitle :: ST.AskPermissionScreenState -> String
getTitle state = case state.props.currentStep of
  Just ST.Overlay -> getString OVERLAY_TO_DRAW_OVER_APPLICATIONS
  Just ST.Battery -> getString BATTERY_OPTIMIZATIONS
  Just ST.AutoStart -> getString AUTO_START_APPLICATION_IN_BACKGROUND
  Just ST.Notifications -> getString NOTIFICATION_ACCESS
  Just ST.LocationPermission -> getString LOCATION_ACCESS
  Nothing -> ""

getDesc :: ST.AskPermissionScreenState -> String
getDesc state = case state.props.currentStep of
  Just ST.Overlay -> getString NEED_IT_TO_SHOW_YOU_INCOMING_RIDE_REQUEST
  Just ST.Battery -> getString NEED_IT_TO_DISABLE_BATTERY_OPTIMIZATION_FOR_THE_APP
  Just ST.AutoStart -> getString NEED_IT_TO_AUTOSTART_YOUR_APP
  Just ST.Notifications -> getString NOTIFICATION_ACCESS_DESC
  Just ST.LocationPermission -> getString $ TO_GET_YOU_RIDES
  Nothing -> ""

getIcon :: ST.AskPermissionScreenState -> String
getIcon state =
  fetchImage FF_ASSET case state.props.currentStep of
    Just ST.LocationPermission -> "ny_ic_location_2"
    Just ST.Overlay -> "ny_ic_overlay_2"
    Just ST.Battery -> "ny_ic_battery_2"
    Just ST.Notifications -> "ny_ic_permission_notification"
    Just ST.AutoStart -> "ny_ic_permission_autostart"
    Nothing -> ""
