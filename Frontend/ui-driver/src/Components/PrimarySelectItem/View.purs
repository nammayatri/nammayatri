{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.PrimarySelectItem.View where

import Prelude (Unit, bind, const, pure, unit, (/=), (==), (<>),($))
import Effect (Effect)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, alpha, background, color, cornerRadius, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onClick, orientation, padding, stroke, text, textSize, textView, weight, width, imageWithFallback)


import Components.PrimarySelectItem.Controller (Action(..), PrimarySelectItemState)
import Font.Style as FontStyle
import Styles.Colors as Color
import Font.Size as FontSize
import Common.Types.App
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))
import Engineering.Helpers.MobilityPrelude(isStrEmpty)

view :: forall w .  (Action  -> Effect Unit) -> PrimarySelectItemState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , padding (Padding 20 30 20 10)
    , onClick (\action -> do
                _<- push action
                pure unit 
                ) (const (OnClick state))
    ][ textView $
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , text state.label
        , color Color.greyTextColor
        , margin (MarginBottom 10)
        ] <> FontStyle.paragraphText TypoGraphy
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER_VERTICAL
        , padding (Padding 20 18 20 18)
        , margin (MarginBottom 5)
        , background Color.white900
        , cornerRadius 4.0  
        , stroke ("1," <> Color.borderColorLight)
        ][ textView $
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , weight 1.0
            , text if isStrEmpty state.selectedItem then state.placeholder else state.selectedItem
            , alpha if state.selectedItem /= "" then 1.0 else 0.33
            , color Color.greyTextColor
            ] <> FontStyle.h2 TypoGraphy
          , imageView
            [ width ( V 15 )
            , height ( V 15 )
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_drop_down"
            ]
        ]
    ]