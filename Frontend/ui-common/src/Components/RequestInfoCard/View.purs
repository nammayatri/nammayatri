{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RequestInfoCard.View where

import Components.RequestInfoCard.Controller (Action(..) , Config, TextConfig)
import Prelude ((*), Unit, ($), const, (/), unit, (-), (<>), (/=))
import Effect (Effect)
import PrestoDOM (PrestoDOM, Accessiblity(..),Orientation(..), Gravity(..), Padding(..), Margin(..), Length(..), margin, padding, orientation, height, width, linearLayout, imageView, imageUrl, text, textView, textSize, fontStyle, gravity, onClick, color, background, cornerRadius, weight, imageWithFallback , visibility, accessibility, accessibilityHint)
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Engineering.Helpers.Commons (screenWidth)
import Common.Types.App (LazyCheck(..))

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
view push state = 
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , padding (Padding 16 0 16 0)
  , gravity CENTER
  , background Color.black9000
  , onClick push $ const BackPressed
  ][ linearLayout
     [ width MATCH_PARENT
     , height WRAP_CONTENT
     , orientation VERTICAL
     , background Color.white900
     , cornerRadius 16.0
     , onClick push $ const NoAction
     ][ 
        linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        ][ 
            linearLayout
            [ width (V ((((screenWidth unit)/3) * 2) - 27))
            , height WRAP_CONTENT
            , orientation VERTICAL
            ][
              genericTextView push state.title
            , genericTextView push state.primaryText
            ]
            , linearLayout
              [ height WRAP_CONTENT
              , weight 1.0
              ][]
            , imageView
              [ width state.imageConfig.width
              , height state.imageConfig.height
              , imageWithFallback state.imageConfig.imageUrl
              , visibility state.imageConfig.visibility
              , margin state.imageConfig.margin
              , padding state.imageConfig.padding
              ]
        ]
        , genericTextView push state.secondaryText
        , textView $
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , color state.buttonConfig.color
          , gravity state.buttonConfig.gravity
          , text state.buttonConfig.text
          , padding state.buttonConfig.padding
          , margin state.buttonConfig.margin
          , onClick push $ const Close
          ] <> (FontStyle.getFontStyle state.buttonConfig.textStyle LanguageStyle)
     ]

  ]

genericTextView :: forall w. (Action -> Effect Unit) -> TextConfig -> PrestoDOM (Effect Unit) w
genericTextView push config = 
  textView $
  [ width config.width
  , height config.height
  , padding config.padding
  , margin config.margin
  , text config.text
  , color config.color
  , visibility config.visibility
  , accessibility $ if config.accessibilityHint /= "" then ENABLE else DISABLE
  , accessibilityHint $ config.accessibilityHint
  ] <> (FontStyle.getFontStyle config.textStyle LanguageStyle)