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
