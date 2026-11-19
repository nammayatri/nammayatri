{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.DropDownWithHeader.View where

import Components.DropDownWithHeader.Controller
import Data.Maybe
import Prelude
import Data.String as DS
import PrestoDOM.List
import Common.Types.App (LazyCheck(..))
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import PrestoDOM (Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), afterRender, alignParentBottom, background, clickable, color, cornerRadius, editText, fontStyle, gravity, height, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onClick, orientation, padding, singleLine, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, root, backgroundColor, alpha)
import PrestoDOM.Properties (lineHeight, cornerRadii)
import PrestoDOM.Types.DomAttributes (Gravity(..), Corners(..))
import Common.Styles.Colors as Color
import Engineering.Helpers.Commons
import Helpers.Utils (fetchImage, FetchImageFrom(..))

view :: forall w a. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding config.boxPadding
    ]
    [ textView
        $ [ height WRAP_CONTENT
          , width MATCH_PARENT
          , visibility if DS.null config.headerText then GONE else VISIBLE
          , text config.headerText
          , color Color.black700
          ]
        <> FontStyle.tags TypoGraphy
    , dropDownView push config
    ]

dropDownView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
dropDownView push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ Padding 16 12 16 12
        , margin config.margin
        , cornerRadius 16.0
        , background config.boxBackground
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        , onClick push $ const $ OnExpand config.selectedContact config.dropDownAction
        ]
        [ textView -- Title
            $ [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , weight 1.0
              , text config.selectedValue.value
              , color Color.black900
              ]
            <> FontStyle.body1 TypoGraphy
        , imageView
            [ height $ V 24
            , width $ V 24
            , imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_chevron_right"
            ]
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ Padding 16 12 16 12
        , cornerRadius 16.0
        , background config.boxBackground
        , orientation VERTICAL
        , visibility config.listVisibility
        , gravity CENTER_VERTICAL
        ]
        $ map
            ( \item ->
                textView -- Title
                  $ [ height WRAP_CONTENT
                    , width MATCH_PARENT
                    , weight 1.0
                    , text item.value
                    , padding $ PaddingVertical 8 8
                    , onClick push $ const $ OnSelect item config.dropDownAction
                    , color Color.black900
                    ]
                  <> FontStyle.body1 TypoGraphy
            )
            config.dropDownOptions
    ]
