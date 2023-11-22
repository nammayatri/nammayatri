{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.GenericHeader.View where

import Components.GenericHeader.Controller (Action(..), Config)
import Effect (Effect)
import Prelude (Unit, const, ($), (==), (<>))
import Font.Style as FontStyle
import Common.Types.App (LazyCheck(..))
import PrestoDOM (Gravity(..), Length(..), Orientation(..), Visibility(..), Accessiblity(..), PrestoDOM, background, clickable, color, disableClickFeedback, fontStyle, gravity, height, imageView, linearLayout, margin, onClick, orientation, padding, text, textSize, textView, visibility, width, imageWithFallback, accessibilityHint, accessibility)
import Engineering.Helpers.MobilityPrelude(isStrEmpty)

view :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
view push config = 
  linearLayout
  [ height config.height
  , width config.width
  , margin config.margin
  , gravity config.gravity
  , padding config.padding
  , orientation config.orientation
  , background config.background
  , accessibility DISABLE
  , clickable config.isClickable
  ][  linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity CENTER
      , accessibility config.prefixImageConfig.accessibility
      , accessibilityHint config.prefixImageConfig.accessibilityHint
      , onClick push $ const PrefixImgOnClick
      , accessibilityHint config.prefixImageConfig.accessibilityHint
      , accessibility ENABLE
      ][ imageView
        [ imageWithFallback config.prefixImageConfig.imageUrl
        , height config.prefixImageConfig.height
        , width config.prefixImageConfig.width
        , margin config.prefixImageConfig.margin
        , padding config.prefixImageConfig.padding
        , accessibility DISABLE
        , visibility config.prefixImageConfig.visibility
        ]
    ]
    , textView $ 
      [ height WRAP_CONTENT
      , width WRAP_CONTENT 
      , text config.textConfig.text
      , margin config.textConfig.margin
      , clickable false
      , accessibility ENABLE
      , accessibilityHint if isStrEmpty config.textConfig.accessibilityHint then config.textConfig.text else config.textConfig.accessibilityHint
      , color config.textConfig.color
      ] <> (FontStyle.getFontStyle config.textConfig.textStyle LanguageStyle)
    , suffixImageLayout config push
  ]

----------------------- suffixImageLayout ---------------------

suffixImageLayout :: forall w . Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
suffixImageLayout config push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity RIGHT
  ][  imageView
      [
        imageWithFallback config.suffixImageConfig.imageUrl
      , height config.suffixImageConfig.height
      , width config.suffixImageConfig.width
      , accessibilityHint config.suffixImageConfig.accessibilityHint
      , accessibility config.suffixImageConfig.accessibility
      , margin config.suffixImageConfig.margin
      , padding config.suffixImageConfig.padding
      , accessibilityHint config.suffixImageConfig.accessibilityHint
      , accessibility ENABLE
      , onClick push $ const SuffixImgOnClick
      , visibility config.suffixImageConfig.visibility
      ]
  ]