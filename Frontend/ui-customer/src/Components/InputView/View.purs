{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.InputView.View where

import Prelude
import Effect (Effect)
import Components.InputView.Controller
import Components.SeparatorView.View as SeparatorView
import Styles.Colors as Color
import Mobility.Prelude (boolToVisibility)
import PrestoDOM (PrestoDOM(..),alpha, Orientation(..), Length(..), Visibility(..), Gravity(..), Padding(..), Margin(..),Accessiblity(..),linearLayout, height, width, orientation, margin, padding, textView, color, background, cornerRadius, weight, text, imageView, imageWithFallback, stroke, gravity, visibility, onChange, onFocus, onClick, selectAllOnFocus, hint, hintColor, cursorColor, pattern, maxLines, singleLine, ellipsize, editText, id, clickable, afterRender, textSize, rippleColor ,accessibilityHint , accessibility)
import Data.Array (mapWithIndex, length)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Engineering.Helpers.Commons (getNewIDWithTag, isTrue)
import Font.Style as FontStyle
import Font.Size as FontSize
import Common.Types.App (LazyCheck(..))
import JBridge (debounceFunction, showKeyboard)
import Resources.Constants (getDelayForAutoComplete)
import Helpers.CommonView (emptyTextView)
import Debug (spy)
import Data.String (null)

view :: forall w. (Action -> Effect Unit) -> InputViewConfig -> PrestoDOM (Effect Unit) w
view push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ PaddingHorizontal 16 16
    , background Color.black900
    ][  if state.headerVisibility then backPressView state push else emptyTextView
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ PaddingVertical 16 16
        , gravity CENTER_VERTICAL
        ][  if not state.headerVisibility then backPressView state push else emptyTextView
          , if state.imageLayoutVisibility == VISIBLE then inputImageView push state else emptyTextView
          , inputLayoutViews push state]
        ]

backPressView :: forall w. InputViewConfig -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
backPressView config push = 
  linearLayout
    [ height MATCH_PARENT
    , width $ if config.headerVisibility then MATCH_PARENT else WRAP_CONTENT
    , padding $ config.backIcon.padding 
    ][  imageView
        [ height $ config.backIcon.height 
        , width $ config.backIcon.width
        , imageWithFallback config.backIcon.imageName
        , rippleColor Color.rippleShade
        , onClick push $ const $ BackPressed
        , accessibility ENABLE
        , accessibilityHint $ "Back"
        ]
      , textView $
        [ text $ config.headerText
        , visibility $ boolToVisibility $ config.headerVisibility
        , color Color.white900  
        , margin $ MarginLeft 8
        ] <> (FontStyle.subHeading2 LanguageStyle)
      , dateTimePickerButton config push
    ]


inputImageView :: forall w. (Action -> Effect Unit) -> InputViewConfig -> PrestoDOM (Effect Unit) w
inputImageView push config =
  let
    lastIndex = (length config.inputView) - 1
  in
    linearLayout
      [ height WRAP_CONTENT
      , width $ config.imageLayoutWidth
      , orientation VERTICAL
      , margin $ config.imageLayoutMargin
      , gravity CENTER
      , visibility config.imageLayoutVisibility
      ]
      (mapWithIndex (\index item -> prefixImageView push (index /= lastIndex) item) config.inputView)
  where
  prefixImageView :: forall w. (Action -> Effect Unit) -> Boolean -> InputView -> PrestoDOM (Effect Unit) w
  prefixImageView push showSeparator config =
    linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , orientation VERTICAL
      , gravity CENTER
      ]
      [ imageView
          [ height $ config.prefixImage.height
          , width $ config.prefixImage.width
          , imageWithFallback $ fetchImage FF_COMMON_ASSET config.prefixImage.imageName
          , visibility $ boolToVisibility $ not $ null config.prefixImage.imageName
          ]
      , if showSeparator then SeparatorView.view (config.imageSeparator) else emptyTextView
      ]
    
inputLayoutViews :: forall w. (Action -> Effect Unit ) -> InputViewConfig -> PrestoDOM (Effect Unit) w
inputLayoutViews push config = 
  linearLayout
    [ height WRAP_CONTENT
    , weight 1.0 
    , padding $ config.inputLayoutPading
    , orientation VERTICAL 
    ]
    ( mapWithIndex 
        ( \index item ->
            if item.isEditable then 
              inputTextField push item
              else 
                nonEditableTextView push item 
        ) 
        (config.inputView))

dateTimePickerButton :: forall w. InputViewConfig -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
dateTimePickerButton config push =
  linearLayout
    [ height MATCH_PARENT
    , weight 1.0
    , gravity RIGHT
    , orientation HORIZONTAL
    , visibility config.suffixButtonVisibility
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , background Color.squidInkBlue
        , cornerRadius 4.0
        , padding $ config.suffixButton.padding
        , gravity config.suffixButton.gravity
        , onClick push $ const DateTimePickerButtonClicked
        , rippleColor Color.rippleShade
        ]
        [ imageView
            [ imageWithFallback $ fetchImage FF_ASSET config.suffixButton.prefixImage
            , height $ V 16
            , width $ V 16
            , margin $ MarginTop 1
            ]
        , textView $
            [ text config.suffixButton.text
            , margin $ MarginHorizontal 4 4
            , color Color.black600
            , accessibility ENABLE
            , accessibilityHint $ if config.suffixButton.accessibilityHint /= "" then config.suffixButton.accessibilityHint else config.suffixButton.text 
            ] <> FontStyle.subHeading2 LanguageStyle
        , imageView
            [ imageWithFallback $ fetchImage FF_ASSET config.suffixButton.suffixImage
            , height $ V 16
            , width $ V 16
            , margin $ MarginTop 1
            ]
        ]
    ]


nonEditableTextView :: forall w. (Action -> Effect Unit ) -> InputView -> PrestoDOM (Effect Unit) w
nonEditableTextView push config = let
  strokeValue = ((if config.inputTextConfig.isFocussed then "1," else "0,") <> Color.yellow900)
  in 
  linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity config.gravity
    , padding $ config.padding 
    , background Color.squidInkBlue
    , margin $ config.inputTextConfig.margin
    , cornerRadius $ config.inputTextConfig.cornerRadius
    , clickable $ config.isClickable 
    , alpha config.alpha
    , onClick push $ const $ TextFieldFocusChanged config.inputTextConfig.id true true
    , rippleColor $ Color.rippleShade
    , stroke $ strokeValue 
    ]
    [  imageView
        [ imageWithFallback $ config.inputTextConfig.prefixImageConfig.imageName
        , height $ config.inputTextConfig.prefixImageConfig.height
        , width $ config.inputTextConfig.prefixImageConfig.width
        , visibility $ config.inputTextConfig.prefixImageVisibility
        ]
      , textView $ 
          [ text $ if config.inputTextConfig.textValue == "" then config.inputTextConfig.placeHolder else config.inputTextConfig.textValue
          , color config.inputTextConfig.textColor
          , height WRAP_CONTENT
          , maxLines 1
          , ellipsize true
          ] <> config.fontStyle
     ]


inputTextField :: forall w. (Action -> Effect Unit ) -> InputView -> PrestoDOM (Effect Unit) w
inputTextField push config =
  linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin config.inputTextConfig.margin 
    ] 
    [ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , background Color.squidInkBlue
      , cornerRadius $ config.inputTextConfig.cornerRadius 
      ][  editText $ 
          [ height $ config.height 
          , weight 1.0
          , padding $ config.padding 
          , color config.inputTextConfig.textColor
          , gravity LEFT
          , background Color.squidInkBlue
          , cornerRadius $ config.inputTextConfig.cornerRadius 
          , text config.inputTextConfig.textValue
          , selectAllOnFocus true
          , singleLine true
          , hint config.inputTextConfig.placeHolder
          , hintColor $ Color.blueGrey
          , pattern "[^\n]*,255"
          , id $ getNewIDWithTag $ config.inputTextConfig.id
          , onChange (\action -> do 
              case action of 
                InputChanged text -> if text /= "false" && text /= config.inputTextConfig.textValue then do 
                  void $ debounceFunction getDelayForAutoComplete push AutoCompleteCallBack config.inputTextConfig.isFocussed
                  void $ push action
                  else pure unit 
                _ -> push action
              pure unit
            ) InputChanged
          , afterRender (\_ -> do 
              if (config.inputTextConfig.isFocussed) then do
                void $ pure $ showKeyboard $ getNewIDWithTag config.inputTextConfig.id 
                pure unit
                else 
                  pure unit
              ) $ const NoAction
          , onFocus 
              (\action -> do 
                case action of 
                  TextFieldFocusChanged _ _ hasFocus -> do 
                    if (isTrue hasFocus) then do 
                      push action
                      else  
                        pure unit
                  _ -> pure unit

              ) $ TextFieldFocusChanged config.inputTextConfig.id true
          , cursorColor Color.yellow900
          ] <> FontStyle.subHeading1 TypoGraphy
        , crossButtonView push config
        ]
    , bottomStrokeView $ boolToVisibility config.inputTextConfig.isFocussed
    ]

crossButtonView :: forall w. (Action -> Effect Unit) -> InputView -> PrestoDOM (Effect Unit) w
crossButtonView push config = 
      linearLayout
        [ height $ V 32 
        , width $ V 32
        , gravity CENTER 
        , padding $ config.clearTextIcon.padding 
        , onClick push $ const $ ClearTextField config.inputTextConfig.id
        , visibility $ boolToVisibility config.canClearText
        ][  imageView
            [ height $ config.clearTextIcon.height
            , width $ config.clearTextIcon.width 
            , imageWithFallback $ fetchImage FF_ASSET config.clearTextIcon.imageName
            , accessibility ENABLE 
            , accessibilityHint "Clear Text Button"
            ]
        ]

bottomStrokeView :: forall w. Visibility -> PrestoDOM (Effect Unit) w
bottomStrokeView bottomStrokeVisibility =
  linearLayout
  [ height $ V 1
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , visibility $ bottomStrokeVisibility
  , background Color.white900
  ][] 
    