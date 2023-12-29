{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module ReactComponents.BottomNavBar.View where

import Common.Types.App
import React.Basic.Hooks (Component, JSX, component, useState, (/\))
import React.Render.CustomBase (imageView, linearLayout, textView)

import Data.Array (mapWithIndex)
import Data.Map (update)
import Data.Monoid.Split (Split(..))
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (toStringJSON)
import JBridge (startLottieProcess, lottieAnimationConfig)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, pure, ($), (&&), (/=), (<>), (==))
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import ReactComponents.BottomNavBar.Controller (Action(..))
import Screens.Types (BottomNavBarState)
import Storage (getValueToLocalNativeStore, KeyStore(..))
import Styles.Colors as Color
import React.Basic.Hooks as React

app :: (Action -> Effect Unit) -> Component BottomNavBarState
app push = do
  component "bottomNavBar" \initialState -> React.do
    state /\ updateState <- useState initialState
    pure $ view push state updateState

view :: (Action -> Effect Unit) -> BottomNavBarState -> ((BottomNavBarState -> BottomNavBarState) -> Effect Unit) -> JSX
view push state updateState =
  linearLayout
    { width: "match_parent"
    , height: "wrap_content"
    , alignParentBottom: "true,-1"
    , gravity: "center"
    }
    [ linearLayout
        { width: "match_parent"
        , height: "match_parent"
        , stroke: "1," <> Color.grey900
        , background: Color.white900
        }
        ( mapWithIndex
            ( \index item ->
                linearLayout
                  { width: "wrap_content"
                  , height: "match_parent"
                  , weight: "1.0"
                  , gravity: "center"
                  , orientation: "vertical"
                  -- , onClick: push (const (OnNavigate item.text))
                  }
                  [ textView $
                      { text: " " <> getString NEW_ <> "✨"
                      , background: Color.blue800
                      , color: Color.white900
                      -- , cornerRadii: Corners 6.0 false false true true
                      , gravity: "center"
                      , padding: "0, 1, 0, 1"
                      , margin: "6, 0, 6, 0"
                      , width: "match_parent"
                      , visibility: if item.showNewBanner then "visible" else "gone"
                      }
                  , linearLayout
                      { width: "60"
                      , height: "wrap_content"
                      , gravity: "center"
                      , padding: "0, " <> (if item.showNewBanner then "0, " else "10, ") <> "0, 10"
                      , orientation: "vertical"
                      }
                      [ if ((item.text == "Alert") && ((getValueToLocalNativeStore ALERT_RECEIVED) == "true") && state.activeIndex /= 3) then
                          lottieLoaderView push state state.activeIndex item.text
                        else
                          imageView
                            { width: "24"
                            , height: "24"
                            , imageWithFallback: if state.activeIndex == index then item.activeIcon else item.defaultIcon
                            }
                      , textView
                          ( { weight: "1.0"
                            , height: "wrap_content"
                            , gravity: "center_horizontal"
                            -- , maxLines: "1"
                            , color: if index == state.activeIndex then Color.black else Color.black600
                            , text: textToString item.text
                            }
                          )
                      ]
                  ]
            )
            state.navButton
        )
    ]
  where
  textToString :: String -> String
  textToString item = case item of
    "Home" -> getString HOME
    "Rides" -> getString RIDES
    "Rankings" -> getString CONTEST
    "Profile" -> getString PROFILE
    "Alert" -> getString MESSAGES
    "Join" -> getString if getValueToLocalNativeStore DRIVER_SUBSCRIBED == "true" then MY_PLAN else PLANS
    _ -> ""

lottieLoaderView :: (Action -> Effect Unit) -> BottomNavBarState -> Int -> String -> JSX
lottieLoaderView push state activeIndex text =
  linearLayout
    { width: "25"
    , height: "25"
    , visibility: if ((getValueToLocalNativeStore ALERT_RECEIVED) == "false") then "gone" else "visible"
    }
    [
    -- lottieAnimationView
    --   { height: "25"
    --   , width: "25"
    --   , id: getIdForScreenIndex activeIndex
    --   , afterRender: \action -> void $ pure $ startLottieProcess lottieAnimationConfig { rawJson = "notification_bell.json", lottieId = (getIdForScreenIndex activeIndex), speed = 1.0 }
    --   , onClick: push (const (OnNavigate text))
    --   }
    ]

-- lottieLoaderView :: forall w. BottomNavBarState -> (Action -> Effect Unit) -> Int -> String -> PrestoDOM (Effect Unit) w
-- lottieLoaderView state push activeIndex text =
--   linearLayout
--     [ width (V 25)
--     , height (V 25)
--     , visibility if ((getValueToLocalNativeStore ALERT_RECEIVED) == "false") then GONE else VISIBLE
--     ]
--     [ lottieAnimationView
--         [ height (V 25)
--         , width (V 25)
--         , id (getIdForScreenIndex activeIndex)
--         , afterRender
--             ( \action ->
--                 void $ pure $ startLottieProcess lottieAnimationConfig { rawJson = "notification_bell.json", lottieId = (getIdForScreenIndex activeIndex), speed = 1.0 }
--             )
--             (const OnNavigate text)
--         ]
--     ]

-- getIdForScreenIndex :: Int -> String
-- getIdForScreenIndex activeIndex = getNewIDWithTag ("NotificationBellAlertView" <> (toStringJSON activeIndex))