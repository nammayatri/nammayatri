module Components.NavBar.View where

import Common.Types.App

import Components.NavBar.Controller (Action(..))
import Data.Array (mapWithIndex)
import Data.Monoid.Split (Split(..))
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage, toStringJSON)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, (==), const, (<>), (&&), bind, ($), pure, unit, (/=), void)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), afterRender, alignParentBottom, background, color, ellipsize, fontStyle, gravity, height, id, imageUrl, imageView, imageWithFallback, linearLayout, lottieAnimationView, margin, maxLines, onClick, orientation, padding, stroke, text, textSize, textView, visibility, weight, width)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Styles.Colors as Color

view :: forall w. (Action -> Effect Unit) -> Int -> PrestoDOM (Effect Unit) w
view push activeIndex =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , stroke $ "1," <> Color.grey900
        , background Color.white900
        ]
        ( mapWithIndex
            ( \index item ->
                linearLayout
                  [ width WRAP_CONTENT
                  , height MATCH_PARENT
                  , weight 1.0
                  , gravity CENTER
                  , onClick push $ const $ NavigateTo index
                  , orientation VERTICAL
                  ]
                  [ linearLayout
                      [ width $ V 60
                      , height WRAP_CONTENT
                      , gravity CENTER
                      , padding $ PaddingVertical 10 10
                      , orientation VERTICAL
                      ]
                      [ imageView
                          [ width $ V 24
                          , height $ V 24
                          , imageWithFallback $ fetchImage FF_ASSET if isActive index then item.activeIcon else item.defIcon
                          ]
                      , textView
                          $ [ weight 1.0
                            , height WRAP_CONTENT
                            , gravity CENTER_HORIZONTAL
                            , maxLines 1
                            , color if isActive index then Color.black else Color.black600
                            , text item.text
                            ]
                          <> FontStyle.tags TypoGraphy
                      ]
                  ]
            )
            [ { text: "Home", defIcon: "ny_ic_nav_home_grey", activeIcon : "ny_ic_nav_home_black" }, { text: "Ticketing", defIcon: "ny_ic_ticket_grey" , activeIcon : "ny_ic_ticket_black"} ]
        )
    ]
    where isActive index = index == activeIndex