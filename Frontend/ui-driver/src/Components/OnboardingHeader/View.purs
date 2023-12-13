{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.OnboardingHeader.View where

import Prelude (Unit, const, unit, map, ($), (/), (<>), (>=))
import Effect (Effect)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, background, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onClick, orientation, padding, stroke, text, textSize, textView, weight, width, imageWithFallback)
import Font.Style as FontStyle
import Styles.Colors as Color
import Font.Size as FontSize
import Components.OnboardingHeader.Controller
import Engineering.Helpers.Commons (screenWidth)
import Language.Strings (getString)
import Language.Types (STR(..))
import Common.Types.App
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))

view :: forall w. (Action -> Effect Unit) -> OnboardingHeaderState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ statusBarView state
    , navigationView state push
    ]

statusBarView :: OnboardingHeaderState -> forall w. PrestoDOM (Effect Unit) w
statusBarView state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , gravity CENTER
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , padding (Padding 10 16 10 0)
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , margin (Margin 0 30 0 20)
        ]
        ( map
            ( \(item) ->
                linearLayout
                  [ width $ V ((screenWidth unit) / 5)
                  , height (V 7)
                  , background if (state.barNumber >= item) then Color.black900 else Color.lightGreyShade
                  , cornerRadius 6.0
                  , margin (Margin 6 0 6 0)
                  ]
                  []
            )
            [ 1, 2, 3, 4 ]
        )
    ]

navigationView :: OnboardingHeaderState -> forall w. (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
navigationView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , padding (Padding 16 16 16 0)
    , onClick push (const BackPressed)
    ]
    [ linearLayout
        [ weight 1.0
        , height MATCH_PARENT
        , width MATCH_PARENT
        ]
        []
    , linearLayout
        [ width WRAP_CONTENT
        , padding (Padding 13 7 13 7)
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , background Color.blue600
        , cornerRadius 5.0
        , stroke ("1," <> Color.blueBtn)
        , onClick push (const TriggerRegModal)
        ]
        [ textView
            $ [ text ((getString STEP) <> state.stepNumber <> "/4")
              , color Color.blueBtn
              ]
            <> FontStyle.paragraphText TypoGraphy
        , imageView
            [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_drop_down"
            , height (V 11)
            , margin (Margin 5 3 0 0)
            , width (V 11)
            ]
        ]
    ]
