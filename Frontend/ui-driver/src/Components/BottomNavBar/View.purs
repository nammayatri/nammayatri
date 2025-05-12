{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.BottomNavBar.View where

import Common.Types.App
import Components.BottomNavBar.Controller
import ConfigProvider

import Data.Array (mapWithIndex)
import Data.Monoid.Split (Split(..))
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (toStringJSON)
import JBridge (startLottieProcess, lottieAnimationConfig)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, (==), const, (<>), (&&), bind, ($), pure, unit, (/=), void)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), afterRender, alignParentBottom, background, color, ellipsize, fontStyle, gravity, height, id, imageUrl, imageView, imageWithFallback, linearLayout, lottieAnimationView, margin, maxLines, onClick, orientation, padding, stroke, text, textSize, textView, visibility, weight, width, rippleColor)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (BottomNavBarState)
import Storage (getValueToLocalNativeStore, KeyStore(..))
import Styles.Colors as Color

view :: forall w . (Action -> Effect Unit) -> BottomNavBarState -> PrestoDOM (Effect Unit) w
view push state = 
    let config = getAppConfig appConfig
    in
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , alignParentBottom "true,-1"
    , gravity CENTER
    ][ linearLayout
       [ width MATCH_PARENT
       , height MATCH_PARENT
       , stroke ("1,"<> Color.grey900)
       , background config.themeColors.navBarBackground
       ](mapWithIndex
         (\index item ->
          linearLayout
          [ width WRAP_CONTENT
          , height MATCH_PARENT
          , weight 1.0
          , gravity CENTER
          , orientation VERTICAL
          , onClick push (const (OnNavigate item.text))
          , rippleColor Color.rippleShade
          ][ textView $
             [ text $ " " <> getString NEW_ <> "✨"
             , background Color.blue800
             , color Color.white900
             , cornerRadii $ Corners 6.0 false false true true
             , gravity CENTER
             , padding $ PaddingVertical 1 1
             , margin $ MarginHorizontal 6 6
             , width MATCH_PARENT
             , visibility if item.showNewBanner then VISIBLE else GONE
             ] <> FontStyle.body18 TypoGraphy
            , linearLayout
              [ width $ V 60
              , height WRAP_CONTENT
              , gravity CENTER
              , padding $ PaddingVertical (if item.showNewBanner then 0 else 10) 10
              , orientation VERTICAL
              ][ if ((item.text == "Alert") && ((getValueToLocalNativeStore ALERT_RECEIVED) == "true") && state.activeIndex /= 3) then
                    lottieLoaderView state push state.activeIndex item.text
                 else
                    imageView
                    [ width (V 24)
                    , height (V 24)
                    , imageWithFallback if state.activeIndex == index then item.activeIcon else item.defaultIcon
                    ]
                  , textView (
                    [ weight 1.0
                    , height WRAP_CONTENT
                    , gravity CENTER_HORIZONTAL
                    , maxLines 1
                    , color if index == state.activeIndex then Color.black else Color.black600
                    , text case item.text of
                              "Home"          -> getString HOME
                              "Rides"         -> getString RIDES
                              "Rankings"      -> getString BENEFITS
                              "Earnings"      -> getString EARNINGS
                              "Profile"       -> getString PROFILE
                              "Alert"         -> getString MESSAGES
                              "Join"          -> getString if getValueToLocalNativeStore DRIVER_SUBSCRIBED == "true" then MY_PLAN else PLANS
                              _               -> ""
                    ] <> FontStyle.tags TypoGraphy)
                ]
           ]
         ) state.navButton
         )
    ]

lottieLoaderView :: forall w. BottomNavBarState -> (Action -> Effect Unit) -> Int -> String -> PrestoDOM (Effect Unit) w
lottieLoaderView state push activeIndex text =
  linearLayout
  [ width (V 25)
  , height (V 25)
  , visibility if((getValueToLocalNativeStore ALERT_RECEIVED) == "false" ) then GONE else VISIBLE
  ][ lottieAnimationView
    [ height (V 25)
    , width (V 25)
    , id (getIdForScreenIndex activeIndex)
    , afterRender
        ( \action ->
            void $ pure $ startLottieProcess lottieAnimationConfig {rawJson = "notification_bell.json", lottieId = (getIdForScreenIndex activeIndex), speed = 1.0 }
        )(const OnNavigate text)
    ]
  ]

getIdForScreenIndex :: Int -> String
getIdForScreenIndex activeIndex = getNewIDWithTag ("NotificationBellAlertView" <> (toStringJSON activeIndex))