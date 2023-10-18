{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.CheckListView.View where

import Prelude (Unit, const, ($), (==), (<>),map)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, color, fontStyle, frameLayout, gravity, height, imageUrl, imageView, linearLayout, margin, onClick, orientation, padding, text, textSize, textView, visibility, weight, width, imageWithFallback,stroke,cornerRadius)
import Components.CheckListView.Controller
import Data.Array (mapWithIndex, length)
import Effect (Effect)
import Font.Style as FontStyle
import Font.Size as FontSize
import Styles.Colors as Color
import Common.Types.App
import Common.Types.App (CheckBoxOptions)
import Helpers.Utils(fetchImage, FetchImageFrom(..))

view :: forall w .  (Action  -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ][
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    ][
      optionView push config
    ]
  ]

optionView :: forall w .  (Action  -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
optionView push config = 
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , padding (PaddingBottom 24)
    ] (mapWithIndex (\index item ->
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background if item.isSelected then Color.blue600 else Color.white900
        , orientation HORIZONTAL
        , margin (Margin 20 10 20 10)
        , stroke if item.isSelected then ("1," <> Color.blue900) else ("1," <> Color.grey700)
        , cornerRadius 2.0
        , onClick push (const (ChangeCheckBoxSate item))
        ][
            linearLayout
            [
            height WRAP_CONTENT
            , width WRAP_CONTENT
            , orientation VERTICAL
            ][
            textView $ [
                height WRAP_CONTENT
                , width WRAP_CONTENT
                , color Color.black800
                , text item.text
                , padding (Padding 15 10 15 3)
                , textSize FontSize.a_14
            ]
            , textView $ [
                height WRAP_CONTENT
                , width WRAP_CONTENT
                , color Color.black800
                , text item.subText
                , padding (Padding 15 0 15 10)
                , textSize FontSize.a_12
            ]
        ]  
        ,linearLayout 
        [
        height WRAP_CONTENT
        , width MATCH_PARENT
        , weight 1.0
        , gravity RIGHT
        ][ 
        checkBoxView config item push
        ] 
    ]
    ) config.optionsProvided)

checkBoxView :: forall w .  Config -> CheckBoxOptions -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
checkBoxView config item push =
    linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , padding (Padding 0 20 20 0)
    ][ frameLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT      
        ][ linearLayout
            [ height (V 18)
            , width (V 18)
            , stroke ("1," <> Color.black900)
            , cornerRadius 2.0
            ][
            imageView
                [ width (V 18)
                , height (V 18)
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_check_box"
                , visibility if item.isSelected then VISIBLE else GONE
                ]
            ]
        ]
    ]

