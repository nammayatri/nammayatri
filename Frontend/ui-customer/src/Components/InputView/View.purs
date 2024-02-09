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
import PrestoDOM (PrestoDOM(..), Orientation(..), Length(..), Visibility(..), Gravity(..), Padding(..), Margin(..), linearLayout, height, width, orientation, margin, padding, textView, color, background, cornerRadius, weight, text, imageView, imageWithFallback, stroke, gravity, visibility, onChange, onFocus, onClick, selectAllOnFocus, hint, hintColor, cursorColor, pattern, maxLines, singleLine, ellipsize, editText, id, afterRender, clickable)
import Data.Array (mapWithIndex, length)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Engineering.Helpers.Commons (getNewIDWithTag, isTrue)
import Font.Style as FontStyle
import Common.Types.App (LazyCheck(..))
import JBridge (debounceFunction, showKeyboard)
import Resources.Constants (getDelayForAutoComplete)

view :: forall w. (Action -> Effect Unit) -> InputViewConfig -> PrestoDOM (Effect Unit) w
view push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , padding $ PaddingHorizontal 16 16
    , background Color.black900 
    ][  backPressView state push
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ PaddingVertical 16 16
        , gravity CENTER_VERTICAL
        ][  inputImageView push state
          , textViews push state]
        ]

backPressView :: forall w. InputViewConfig -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
backPressView config push = 
  linearLayout
    [ height MATCH_PARENT
    , width WRAP_CONTENT
    , padding $ config.backIcon.padding 
    , onClick push $ const $ BackPress
    , orientation HORIZONTAL
    ][  imageView
        [ height $ config.backIcon.height 
        , width $ config.backIcon.width
        , imageWithFallback config.backIcon.imageName
        ]
      , textView $
        [ text $ config.headerText
        , visibility $ boolToVisibility $ config.headerVisibility
        , color Color.white900  
        , margin $ MarginLeft 8
        ] <> (FontStyle.subHeading2 LanguageStyle)
    ]


inputImageView :: forall w. (Action -> Effect Unit ) -> InputViewConfig -> PrestoDOM (Effect Unit) w
inputImageView push config = let 
  lastIndex = (length config.inputView) - 1
  in
  linearLayout
    [ height WRAP_CONTENT
    , width $ config.imageLayoutWidth 
    , orientation VERTICAL
    , margin $ config.imageLayoutMargin
    , gravity CENTER
    ]
    ( mapWithIndex 
        ( \index item ->
            linearLayout
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , orientation VERTICAL 
              , gravity CENTER
              ]
              [ prefixImageView item.prefixImage
              , if index /= lastIndex then SeparatorView.view (item.imageSeparator) else textView[height $ V 0, visibility GONE]
              ]
        ) 
        (config.inputView))
  where 
    prefixImageView :: forall w. ImageConfig -> PrestoDOM (Effect Unit) w
    prefixImageView config =
      imageView
        [ height $ config.height
        , width $ config.width
        , imageWithFallback $ fetchImage FF_COMMON_ASSET config.imageName 
        ]
      
textViews :: forall w. (Action -> Effect Unit ) -> InputViewConfig -> PrestoDOM (Effect Unit) w
textViews push config = 
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

nonEditableTextView :: forall w. (Action -> Effect Unit ) -> InputView -> PrestoDOM (Effect Unit) w
nonEditableTextView push config = let
  strokeValue = ((if config.isFocussed then "1," else "0,") <> Color.yellow900)
  in 
  linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    , padding $ config.padding 
    , background Color.squidInkBlue
    , margin $ config.margin
    , cornerRadius $ config.cornerRadius 
    , onClick push $ const $ (TextFieldFocusChanged config.id true false)
    , clickable $ config.isClickable 
    , stroke $ strokeValue 
    ]
    [  textView $ 
          [ text $ config.textValue
          , color Color.black600
          , width MATCH_PARENT
          , height WRAP_CONTENT
          , maxLines 1
          , ellipsize true
          ] <> (FontStyle.body6 LanguageStyle)
     ]


inputTextField :: forall w. (Action -> Effect Unit ) -> InputView -> PrestoDOM (Effect Unit) w
inputTextField push config = 
  linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin config.margin 
    ] 
    [ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , background Color.squidInkBlue
      , cornerRadius $ config.cornerRadius 
      ][  editText $ 
          [ height $ config.height 
          , weight 1.0
          , padding $ config.padding 
          , color Color.black600
          , gravity LEFT
          , background Color.squidInkBlue
          , cornerRadius $ config.cornerRadius 
          , text config.textValue
          , selectAllOnFocus true
          , singleLine true
          , hint config.placeHolder
          , hintColor $ Color.blueGrey
          , pattern "[^\n]*,255"
          , id $ getNewIDWithTag $ config.id
          , onChange (\action -> do 
              void $ debounceFunction getDelayForAutoComplete push AutoCompleteCallBack config.isFocussed
              void $ push action
            ) InputChanged
          , afterRender (\ _ ->  do 
              if (config.isFocussed) then do 
                void $ pure $ showKeyboard $ getNewIDWithTag $ config.id
                pure unit
                else pure unit) $ const NoAction
          , onFocus 
              (\action -> do 
                case action of 
                  TextFieldFocusChanged _ _ hasFocus -> do 
                    if (isTrue hasFocus) then do 
                      push action
                      else  
                        pure unit
                  _ -> pure unit

              ) $ TextFieldFocusChanged config.id true
          , cursorColor Color.yellow900
          ] <> FontStyle.body6 LanguageStyle
        , crossButtonView push config
        ]
    , bottomStrokeView $ boolToVisibility config.isFocussed
    ]

crossButtonView :: forall w. (Action -> Effect Unit) -> InputView -> PrestoDOM (Effect Unit) w
crossButtonView push config = 
      linearLayout
        [ height $ V 32 
        , width $ V 32
        , gravity CENTER 
        , padding $ config.clearTextIcon.padding 
        , onClick push $ const $ ClearTextField config.id
        , visibility $ boolToVisibility config.canClearText
        ][  imageView
            [ height $ config.clearTextIcon.height
            , width $ config.clearTextIcon.width 
            , imageWithFallback $ fetchImage FF_ASSET config.clearTextIcon.imageName
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
    
