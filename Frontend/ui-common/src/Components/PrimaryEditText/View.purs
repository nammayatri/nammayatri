{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.PrimaryEditText.View where

import Prelude (Unit, ($), (<>), (==), (&&), not)
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Array (mapWithIndex)
--import Data.String ()
import Common.Types.App
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Engineering.Helpers.Commons (os)
import Components.PrimaryEditText.Controller (Action(..), Config)
import PrestoDOM (InputType(..),Gravity(..), Length(..), Orientation(..), PrestoDOM, Visibility(..), alpha, background, color, cornerRadius, editText, fontStyle, gravity, height, hint, hintColor, imageUrl, imageView, lineHeight, letterSpacing, linearLayout, margin, onChange, orientation, padding, pattern, singleLine, stroke, text, textSize, textView, visibility, weight, width, id, inputType, multiLineEditText, maxLines, inputTypeI, onFocus, clickable)

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
  textView
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , textSize config.topLabel.textSize
    , text config.topLabel.text
    , color config.topLabel.color
    , fontStyle config.topLabel.fontStyle
    , gravity config.topLabel.gravity
    , lineHeight "28"
    , singleLine true
    , margin config.topLabel.margin
    , alpha config.topLabel.alpha
    ]  


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
          [constantField push config, editTextView push config]
          else [editTextView push config]
          )


constantField :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
constantField push config = 
  textView
  [ width config.constantField.width
  , height config.constantField.height
  , gravity config.constantField.gravity
  , text config.constantField.text
  , fontStyle config.constantField.fontStyle
  , textSize config.constantField.textSize
  , color config.constantField.color
  , padding config.constantField.padding
  , margin config.constantField.margin
  , visibility if config.showConstantField then VISIBLE else GONE
  ]



editTextView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w 
editTextView push config = 
  (if( os == "IOS" && (not config.editText.singleLine )) then multiLineEditText else editText) 
  $ [ height config.height
  , width config.width
  , id config.id
  , weight 1.0
  , textSize config.editText.textSize
  , color config.editText.color
  , fontStyle config.editText.fontStyle
  , text config.editText.text
  , hint config.editText.placeholder
  , singleLine config.editText.singleLine
  , hintColor config.editText.placeholderColor
  , margin config.editText.margin
  , background config.background
  , padding config.editText.padding
  , onChange push $ TextChanged config.id
  , gravity config.editText.gravity
  , letterSpacing config.editText.letterSpacing
  , alpha config.editText.alpha
  , onFocus push $ FocusChanged
  ] 
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
  <> if config.editText.capsLock then [inputTypeI 4097] else []
  <> if not config.editText.enabled then if os == "IOS" then [clickable false] else [inputTypeI 0] else[]

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
      , textView
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , textSize config.errorLabel.textSize
        , text config.errorLabel.text
        , color config.errorLabel.color
        , fontStyle config.errorLabel.fontStyle
        , gravity config.errorLabel.gravity
        , margin config.errorLabel.margin
        , lineHeight "28"
        , singleLine true
        , visibility if config.showErrorLabel then VISIBLE else GONE
        , alpha config.errorLabel.alpha
        ]
    ]

textBoxes :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
textBoxes push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity LEFT
  --, margin (Margin 0 20 0 20)
  , clickable false
  ](mapWithIndex (\index item ->
      textView
      [ width (V 20)
      , height (V 20)
      , color Color.blue600--Color.greyTextColor
      --, text ( DS.take 1 (DS.drop index state.data.otp) )
      , textSize FontSize.a_14
      , fontStyle $ FontStyle.bold LanguageStyle
      , gravity CENTER
      , cornerRadius 4.0
      , stroke ("1," <> Color.borderColorLight) --if (state.props.isValid ) then Color.textDanger else if state.data.focusedIndex == index then Color.highlightBorderColor else Color.borderColorLight )
      --, margin (Margin ((EHC.screenWidth unit)/30) 0 ((EHC.screenWidth unit)/30) 0)
      --, onClick push (const (OnclickTextBox index))
      ]) [1,2,3,4])