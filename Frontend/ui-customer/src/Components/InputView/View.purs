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
import PrestoDOM (layoutGravity, PrestoDOM(..), Orientation(..), Length(..), Visibility(..), Gravity(..), Padding(..), Margin(..), linearLayout, height, width, orientation, margin, padding, textView, color, background, cornerRadius, weight, text, imageView, imageWithFallback, stroke, gravity, visibility, onChange, onFocus, onClick, selectAllOnFocus, hint, hintColor, cursorColor, pattern, maxLines, singleLine, ellipsize, editText, id, afterRender, clickable, relativeLayout, frameLayout)
import Data.Array (mapWithIndex, length, head)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Engineering.Helpers.Commons (getNewIDWithTag, isTrue)
import Font.Style as FontStyle
import Common.Types.App (LazyCheck(..))
import JBridge (debounceFunction, showKeyboard)
import Resources.Constants (getDelayForAutoComplete)
import Debug(spy)
import Data.Maybe

view :: forall w. (Action -> Effect Unit) -> InputViewConfig -> PrestoDOM (Effect Unit) w
view push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , padding $ PaddingHorizontal 16 16
    , background Color.black900 
    ][ backPressView state push
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ PaddingVertical 16 16
        , gravity CENTER_VERTICAL
        ][ textViews push state]
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

      
textViews :: forall w. (Action -> Effect Unit ) -> InputViewConfig -> PrestoDOM (Effect Unit) w
textViews push config =
  frameLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , padding $ config.inputLayoutPading
  , gravity CENTER_VERTICAL
  ][
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL 
    ][inputImageView2 push config]
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    ( mapWithIndex 
        ( \index item ->
            if item.isEditable then 
              inputTextField push item config index
              else 
              nonEditableTextView push item config index
        ) 
        (config.inputView))
  ]

inputImageView2 push config =
  let
    len = length (config.inputView)
    textHeight = case head config.inputView of 
                  Nothing -> 37
                  Just item -> getHeight item.height
    marginVertical = case head config.inputView of 
                  Nothing -> 17
                  Just item -> getVerticalMargins item.margin
    offset = textHeight / 2
    marginTop = (2 * marginVertical)
    maxHeight = ((textHeight + marginVertical) * len) + len
    bottomPadding = (offset)
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , margin $ MarginVertical (marginTop)  offset
      , orientation VERTICAL
      ][(SeparatorView.view (separatorConfig { count = 10 * len, layoutHeight = V $ maxHeight - bottomPadding - marginTop}))]


nonEditableTextView :: forall w. (Action -> Effect Unit ) -> InputView -> InputViewConfig -> Int -> PrestoDOM (Effect Unit) w
nonEditableTextView push config config' index = let
  strokeValue = ((if config.isFocussed then "1," else "0,") <> Color.yellow900)
  in 
  relativeLayout [
    height WRAP_CONTENT
  , width MATCH_PARENT
  ][ linearLayout[
      height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER]
    [ linearLayout [ 
        height WRAP_CONTENT
      , weight 1.0
      , orientation HORIZONTAL
      , margin $ config.margin
      ] 
    [ prefixDotImageView push config (length config'.inputView)
    , linearLayout 
        [ height config.height
        , width MATCH_PARENT
        , gravity CENTER
        , padding $  config.padding
        , background Color.squidInkBlue
        , margin $ MarginLeft 5  
        , cornerRadius $ config.cornerRadius 
        , onClick push $ const $ (TextFieldFocusChanged config.id true index false)
        , clickable $ config.isClickable 
        , stroke $ strokeValue 
        ]
        [  textView $ 
              [ text $ config.hint
              , color Color.black600
              , padding $ Padding 5 5 5 5 
              , width MATCH_PARENT
              , height WRAP_CONTENT
              , maxLines 1
              , ellipsize true
              ] <> (FontStyle.body6 LanguageStyle)
        ]]
    , postfixImageView push config (length config'.inputView)
  ]
  ]


inputTextField :: forall w. (Action -> Effect Unit ) -> InputView -> InputViewConfig -> Int -> PrestoDOM (Effect Unit) w
inputTextField push config config' index =
  let _ = spy "input text field being rendered" config
      _ = spy "input text field being rendered" index
      _ = spy "input text field being rendered" config'
  in
  relativeLayout [
    height WRAP_CONTENT
  , width MATCH_PARENT
  ][linearLayout[
      height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER
  ][  linearLayout 
      [ height WRAP_CONTENT
      , weight 1.0
      , orientation HORIZONTAL
      , margin $ config.margin
      ] 
      [ prefixDotImageView push config (length config'.inputView)
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.squidInkBlue
        , margin $ MarginLeft 5
        , orientation VERTICAL
        , cornerRadius $ config.cornerRadius 
        ][  editText $
            [ height $ config.height 
            , weight 1.0
            , padding $ Padding 5 5 5 5 
            , margin $ config.inputTextViewContainerMargin
            , color Color.black600
            , gravity LEFT
            , background Color.squidInkBlue
            , cornerRadius $ config.cornerRadius 
            , selectAllOnFocus true
            , singleLine true
            -- , text config.textValue
            , hint (if config.index /= 0 && config.index == length (config'.inputView) - 1 then config.placeHolder else if config.index == 0 then "Start" else "Add Stop")
            , hintColor $ Color.blueGrey
            , pattern "[^\n]*,255"
            , id $ getNewIDWithTag $ config.id
            , onChange (\action -> do 
                void $ debounceFunction getDelayForAutoComplete push AutoCompleteCallBack config.isFocussed
                void $ push action
              ) $ (InputChanged index)
            , afterRender (\ _ ->  do 
                if (config.isFocussed) then do 
                  void $ pure $ showKeyboard $ getNewIDWithTag $ config.id
                  pure unit
                  else pure unit) $ const NoAction
            , onFocus 
                (\action -> do 
                  case action of 
                    TextFieldFocusChanged _ _ index hasFocus  -> do 
                      if (isTrue hasFocus) then do 
                        push action
                        else  
                          pure unit
                    _ -> pure unit

                ) $ TextFieldFocusChanged config.id true index 
            , cursorColor Color.yellow900 
            ] <> FontStyle.body6 LanguageStyle
              <> if config.destination == "" then [] else [text config.destination]
      , bottomStrokeView $ boolToVisibility $ config.isFocussed
      ]
      ],
      postfixImageView push config (length config'.inputView)
    ]
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
  , visibility bottomStrokeVisibility
  , background Color.white900
  ][] 
    
postfixImageView :: forall w. (Action -> Effect Unit ) -> InputView -> Int -> PrestoDOM (Effect Unit) w
postfixImageView push config len = 
  linearLayout[
    width WRAP_CONTENT,
    height MATCH_PARENT,
    margin $ MarginTop 12
  ][
    linearLayout 
    [ width config.postfixImageConfig.layoutWidth 
    , height config.postfixImageConfig.layoutHeight
    , cornerRadius config.postfixImageConfig.layoutCornerRadius
    , gravity CENTER
    , padding config.postfixImageConfig.layoutPadding
    , margin config.postfixImageConfig.layoutMargin 
    , background config.postfixImageConfig.layoutColor
    , onClick push $ const $ (determineAction config len) 

    ][ imageView
      [ height config.postfixImageConfig.height 
      , width config.postfixImageConfig.width 
      , imageWithFallback $ fetchImage FF_COMMON_ASSET (if config.index == 0 then "" else if config.index == len-1 then "ny_ic_add" else "ny_ic_cross_white") 
      ]
    ]
  ]
  where
    determineAction :: InputView -> Int -> Action
    determineAction inputView len =
      if inputView.index == len - 1 && inputView.index /= 0
        then AddRemoveStopAction "D" inputView.index 
        else AddRemoveStopAction "ADD" inputView.index 

prefixDotImageView :: forall w. (Action -> Effect Unit ) -> InputView -> Int -> PrestoDOM (Effect Unit) w
prefixDotImageView push config len =
  linearLayout
    [ height $ V 16
    , width $ V 16
    , orientation VERTICAL
    , gravity CENTER_VERTICAL
    , layoutGravity "center_vertical"
    ][
      imageView
        [ height $ V 12
        , width $ V 12
        , margin $ config.inputTextViewContainerMargin
        , imageWithFallback $
            if config.index == 0 then
              "ny_ic_green_circle"
            else if config.index == len-1 then
              "ny_ic_red_circle"
            else
              "ny_ic_grey_circle"
        ]
    ]

inputImageView :: forall w. (Action -> Effect Unit ) -> InputView -> InputViewConfig -> Int -> PrestoDOM (Effect Unit) w
inputImageView push config config' index= let 
  lastIndex = (length config'.inputView) - 1
  in
  linearLayout
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , orientation VERTICAL 
              , gravity CENTER
              ][ (SeparatorView.view (config.imageSeparator)) 
              ]

getVerticalMargins margin = 
  case margin of
    Margin _ mt _ mb -> mt + mb
    MarginBottom mb -> mb
    MarginHorizontal _ _ -> 0
    MarginVertical mt mb -> mt + mb
    MarginLeft _ -> 0
    MarginRight _ -> 0
    MarginTop mt -> mt

getHeight height = 
  case height of
    V h -> h
    _ -> 37
