{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Components.InfoBox.View where

import Components.InfoBox.Controller
import Data.Maybe
import Prelude
import PrestoDOM.List
import Common.Types.App (LazyCheck(..))
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import PrestoDOM (Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), relativeLayout, afterRender, alignParentBottom, background, clickable, color, cornerRadius, editText, fontStyle, gravity, height, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onClick, orientation, padding, singleLine, stroke, text, textFromHtml, textSize, textView, visibility, weight, width, root, backgroundColor, alpha)
import PrestoDOM.Properties (lineHeight, cornerRadii)
import PrestoDOM.Types.DomAttributes (Gravity(..), Corners(..))
import Common.Styles.Colors as Color
import Engineering.Helpers.Commons
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Data.String as String
import Mobility.Prelude (boolToVisibility)

view :: forall w a. (a -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  relativeLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin config.margin --Config needs to be added
    , cornerRadius 16.0
    , background config.backgroundColor
    , visibility config.visibility
    , orientation VERTICAL
    , gravity CENTER_VERTICAL
    ]
    [ internalView VISIBLE
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , visibility $ boolToVisibility $ config.disabled
        , background Color.transparentHigh
        ]
        [ internalView INVISIBLE ]
    ]
  where
  internalView componentVisibility =
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , visibility componentVisibility
      , padding $ Padding 16 16 16 16
      , orientation VERTICAL
      ]
      [ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          ]
          [ imageView -- Image
              [ height $ V 24
              , width $ V 24
              , visibility $ boolToVisibility $ not String.null config.icon
              , margin $ Margin 0 0 8 0
              , imageWithFallback $ fetchImage GLOBAL_COMMON_ASSET config.icon
              ]
          , textView -- Title
              $ [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text $ config.text
                , color config.titleColor
                ]
              <> FontStyle.body32 TypoGraphy
          ]
      , textView
          $ [ height WRAP_CONTENT
            , width MATCH_PARENT
            , visibility $ boolToVisibility $ not String.null config.subTitle
            , margin $ MarginTop 8
            , text $ config.subTitle
            , color Color.black700
            ]
          <> FontStyle.body5 TypoGraphy
      ]
