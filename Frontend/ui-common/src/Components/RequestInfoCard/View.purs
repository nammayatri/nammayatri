{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.RequestInfoCard.View where

import Components.RequestInfoCard.Controller (Action(..) , Config, TextConfig, ImageConfig, dummyImageConfig)
import Prelude ((*), Unit, ($), const, (/), unit, (-), (<>), (/=), map, (>))
import Effect (Effect)
import PrestoDOM (PrestoDOM, Accessiblity(..),Orientation(..), Gravity(..), Padding(..), Margin(..), Length(..), margin, padding, orientation, height, width, linearLayout, imageView, imageUrl, text, textView, textSize, fontStyle, gravity, onClick, color, background, cornerRadius, weight, imageWithFallback , visibility, accessibility, accessibilityHint, textFromHtml)
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Engineering.Helpers.Commons (screenWidth)
import Common.Types.App (LazyCheck(..))
import Data.String as DS
import Data.Array as DA
import Mobility.Prelude (boolToVisibility)
import Data.Maybe (isJust, Maybe(..))

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
view push state = 
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , padding state.padding
  , gravity state.gravity
  , background state.backgroundColor
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
            ,linearLayout
            [ 
              height $ V 1
             , width MATCH_PARENT
              , background Color.grey700
              , visibility $ state.subTitle.visibility
            ][]
            , genericTextView push state.subTitle
            ]
            , linearLayout
              [ height WRAP_CONTENT
              , weight 1.0
              ][]
            , genericImageView state.imageConfig
        ]
        , bulletPoints push state
        , genericImageView state.infoImageConfig
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
  , textFromHtml config.text
  , color config.color
  , visibility config.visibility
  , accessibility $ if DS.null config.accessibilityHint then DISABLE else ENABLE
  , accessibilityHint $ config.accessibilityHint
  ] <> (FontStyle.getFontStyle config.textStyle LanguageStyle)

bulletPoints :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
bulletPoints push config = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin $ Margin 20 12 0 0
  , padding $ Padding 0 0 15 0
  , visibility $ boolToVisibility $ DA.length config.bulletPoints > 0
  ](map (\item -> bulletPoint push item) config.bulletPoints) 

bulletPoint :: forall w. (Action -> Effect Unit) -> String -> PrestoDOM (Effect Unit) w
bulletPoint push point = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ MarginTop 5
  ][ linearLayout
      [ width $ V 7
      , height $ V 7
      , cornerRadius 3.5
      , background Color.black700
      , margin $ MarginTop 8
      ][]
    , textView $
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ MarginLeft 8
      , text point
      ] <> FontStyle.paragraphText TypoGraphy
  ]

genericImageView :: forall w. ImageConfig -> PrestoDOM (Effect Unit) w
genericImageView imageConfig=
  imageView
  [ width imageConfig.width
  , height imageConfig.height
  , imageWithFallback imageConfig.imageUrl
  , visibility imageConfig.visibility
  , margin imageConfig.margin
  , padding imageConfig.padding
  ]
