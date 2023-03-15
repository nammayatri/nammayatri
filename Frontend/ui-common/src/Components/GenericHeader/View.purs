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
import Prelude (Unit, const, ($), (==))
import PrestoDOM (Gravity(..), Length(..), Orientation(..), Visibility(..), PrestoDOM, background, clickable, color, disableClickFeedback, fontStyle, gravity, height, imageView, linearLayout, margin, onClick, orientation, padding, text, textSize, textView, visibility, width, imageWithFallback)

view :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
view push config = 
  linearLayout
  [ height config.height
  , width config.width
  , margin config.margin
  , gravity config.gravity
  , padding config.padding
  , orientation HORIZONTAL 
  , background config.background
  , clickable config.isClickable
  ][  linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity CENTER
      , onClick push $ const PrefixImgOnClick
      ][ imageView
        [ imageWithFallback config.prefixImageConfig.imageUrl
        , height config.prefixImageConfig.height
        , width config.prefixImageConfig.width
        , margin config.prefixImageConfig.margin
        , padding config.prefixImageConfig.padding
        , visibility config.prefixImageConfig.visibility
        ]
    ]
    , textView
      [ height WRAP_CONTENT
      , width WRAP_CONTENT 
      , text config.textConfig.text
      , textSize config.textConfig.textSize
      , margin config.textConfig.margin
      , fontStyle config.textConfig.fontStyle
      , color config.textConfig.color
      ]
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
      , margin config.suffixImageConfig.margin
      , padding config.suffixImageConfig.padding
      , onClick push $ const SuffixImgOnClick
      , visibility config.suffixImageConfig.visibility
      ]
  ]