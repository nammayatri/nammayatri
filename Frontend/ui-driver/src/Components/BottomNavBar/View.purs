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

import Data.Array (mapWithIndex)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, (==), const, (<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), PrestoDOM, alignParentBottom, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onClick, orientation, stroke, text, textSize, textView, weight, width, imageWithFallback)
import Screens.Types (BottomNavBarState)
import Styles.Colors as Color

view :: forall w . (Action -> Effect Unit) -> BottomNavBarState -> PrestoDOM (Effect Unit) w
view push state = 
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , alignParentBottom "true,-1"
    , gravity CENTER
    , stroke ("1,"<> Color.borderColorLight)
    ][ linearLayout
       [ width MATCH_PARENT
       , height MATCH_PARENT
       , margin (Margin 0 10 0 10)
       ](mapWithIndex 
         (\index item -> 
          linearLayout
          [ width WRAP_CONTENT
          , height MATCH_PARENT
          , weight 1.0
          , orientation VERTICAL
          , gravity CENTER
          , onClick push (const (OnNavigate item.text))
          ][ imageView 
             [ width (V 24)
             , height (V 24)
             , imageWithFallback if state.activeIndex == index then item.activeIcon else item.defaultIcon
             ]
           , textView (
             [ width WRAP_CONTENT
             , height WRAP_CONTENT
             , color if index == state.activeIndex then Color.black else Color.black600
             , text case item.text of 
                      "Home"          -> getString HOME
                      "Rides"         -> getString RIDES
                      "Contest"       -> getString CONTEST
                      "Profile"       -> getString PROFILE
                      "Alert"         -> getString ALERTS
                      _               -> ""
             ] <> FontStyle.tags TypoGraphy)
           ]
         ) state.navButton
         )
    ]
