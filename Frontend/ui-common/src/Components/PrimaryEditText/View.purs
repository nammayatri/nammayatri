{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.PrimaryEditText.View where

import Prelude (Unit, ($), (<>), (==), (&&), not, bind, pure, unit)
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Engineering.Helpers.Commons (os, setText)
import Components.PrimaryEditText.Controller (Action(..), Config)
import PrestoDOM (InputType(..),Gravity(..), Length(..), Orientation(..), PrestoDOM, Visibility(..), Accessiblity(..), alpha, background, color, cornerRadius, editText, fontStyle, gravity, height, hint, hintColor, imageUrl, imageView, lineHeight, letterSpacing, linearLayout, margin, onChange, orientation, padding, pattern, singleLine, stroke, text, textSize, textView, visibility, weight, width, id, inputType, multiLineEditText, maxLines, inputTypeI, onFocus, clickable, separator, separatorRepeat, accessibilityHint, accessibility)
import Font.Style as FontStyle
import Common.Types.App
import Data.String as DS

view :: forall w .  (Action  -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config = 
  linearLayout 
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin config.margin
    ][  topLabelView config
      , editTextLayout push config
      , errorLabelLayout config
      ]

topLabelView :: forall w . Config -> PrestoDOM (Effect Unit) w
topLabelView config = 
  textView $ 
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , text config.topLabel.text
    , color config.topLabel.color
    , gravity config.topLabel.gravity
    , lineHeight "28"
    , singleLine true
    , margin config.topLabel.margin
    , alpha config.topLabel.alpha
    , accessibility config.topLabel.accessibility
    , visibility config.topLabel.visibility
    ] <> (FontStyle.getFontStyle config.topLabel.textStyle LanguageStyle)


editTextLayout :: forall w .  (Action  -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
editTextLayout push config = 
  linearLayout
    [ height config.height
    , width config.width
    , background config.background
    , cornerRadius config.cornerRadius
    , gravity CENTER_VERTICAL
    , stroke if config.showErrorLabel then config.warningStroke else if config.editText.focused then config.focusedStroke else config.stroke
    ](  if config.showConstantField then 
          [constantField push config, editTextView push config ] 
          else [editTextView push config])


constantField :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
constantField push config = 
  textView $ 
  [ width config.constantField.width
  , height config.constantField.height
  , gravity config.constantField.gravity
  , text config.constantField.text
  , color config.constantField.color
  , padding config.constantField.padding
  , margin config.constantField.margin
  , visibility if config.showConstantField then VISIBLE else GONE
  ] <> (FontStyle.getFontStyle config.constantField.textStyle LanguageStyle)



editTextView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
editTextView push config = 
  (if( os == "IOS" && (not config.editText.singleLine )) then multiLineEditText else editText) 
  $ [ height config.height
  , width config.width
  , id config.id
  , weight 1.0
  , color config.editText.color
  , text config.editText.text
  , hint config.editText.placeholder
  , singleLine config.editText.singleLine
  , hintColor config.editText.placeholderColor
  , accessibility ENABLE
  , accessibilityHint if config.editText.text == "" then (if config.editText.accessibilityHint == "" then config.editText.placeholder else config.editText.accessibilityHint ) else if (config.type == "number") then (DS.replaceAll (DS.Pattern "") (DS.Replacement "-") (config.editText.text)) else config.editText.text
  , margin config.editText.margin
  , background config.background
  , padding config.editText.padding
  , onChange push $ TextChanged config.id
  , gravity config.editText.gravity
  , letterSpacing config.editText.letterSpacing
  , alpha config.editText.alpha
  , onFocus push $ FocusChanged
  ] <> (FontStyle.getFontStyle config.editText.textStyle LanguageStyle)
    <> (case config.editText.pattern of 
        Just _pattern -> case config.type of 
                          "text" -> [pattern _pattern
                                    , inputType TypeText]
                          "number" -> [ pattern _pattern
                                      , inputType Numeric]
                          "password" -> [ pattern _pattern
                                      , inputType Password]
                          _    -> [pattern _pattern]
        Nothing -> case config.type of 
                          "text" -> [inputType TypeText]
                          "number" -> [inputType Numeric]
                          "password" -> [inputType Password]
                          _    -> []) 
  <> (if config.editText.capsLock then [inputTypeI 4097] else [])
  <> (if not config.editText.enabled then if os == "IOS" then [clickable false] else [inputTypeI 0] else[])
  <> (if config.editText.separator == "" then [] else [
    separator config.editText.separator
  , separatorRepeat config.editText.separatorRepeat
  ])

errorLabelLayout :: forall w . Config -> PrestoDOM (Effect Unit) w
errorLabelLayout config =
  linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    ][  imageView
        [ height config.errorImageConfig.height
        , width config.errorImageConfig.width
        , imageUrl config.errorImageConfig.imageUrl
        , margin config.errorImageConfig.margin
        , padding config.errorImageConfig.padding
        , visibility if config.showErrorImage then VISIBLE else GONE
        ]
      , textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text config.errorLabel.text
        , color config.errorLabel.color
        , gravity config.errorLabel.gravity
        , margin config.errorLabel.margin
        , lineHeight "28"
        , singleLine true
        , visibility if config.showErrorLabel then VISIBLE else GONE
        , alpha config.errorLabel.alpha
        ] <> (FontStyle.getFontStyle config.errorLabel.textStyle LanguageStyle)
    ]