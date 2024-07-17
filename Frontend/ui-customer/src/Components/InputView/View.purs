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
import PrestoDOM (PrestoDOM(..), Orientation(..), Length(..), Visibility(..), Gravity(..), Padding(..), Margin(..), linearLayout, height, width, orientation, margin, padding, textView, color, background, cornerRadius, weight, text, imageView, imageWithFallback, stroke, gravity, visibility, onChange, onFocus, onClick, selectAllOnFocus, hint, hintColor, cursorColor, pattern, maxLines, singleLine, ellipsize, editText, id, clickable, afterRender, textSize, rippleColor, layoutGravity, relativeLayout, frameLayout, alignParentBottom)
import Data.Array (mapWithIndex, length, head)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Engineering.Helpers.Commons (getNewIDWithTag, isTrue)
import Font.Style as FontStyle
import Font.Size as FontSize
import Common.Types.App (LazyCheck(..))
import JBridge (debounceFunction, showKeyboard)
import Resources.Constants (getDelayForAutoComplete)
import Helpers.CommonView (emptyTextView)
import Debug (spy)
import Data.Maybe

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
        -- , background Color.green900
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
      -- , background Color.blue900
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
          ]
      -- , if showSeparator then SeparatorView.view (config.imageSeparator) else emptyTextView
      ]
    
inputLayoutViews :: forall w. (Action -> Effect Unit ) -> InputViewConfig -> PrestoDOM (Effect Unit) w
inputLayoutViews push config = 
  frameLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , padding $ config.inputLayoutPading
  , gravity CENTER_VERTICAL
  ][linearLayout
    [ height WRAP_CONTENT
    -- , weight 1.0 
    -- , padding $ config.inputLayoutPading
    , orientation VERTICAL
    ][inputImageView2 push config]
    , relativeLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      -- , orientation VERTICAL
      ]
    ( mapWithIndex 
        ( \index item ->
            if item.isEditable then 
              inputTextField push item config index
              else 
                nonEditableTextView push item config index
            -- relativeLayout
            --   [ height WRAP_CONTENT
            --   , width MATCH_PARENT
            --   -- , position (PositionAbsolute 0 0 0 0) -- Ensure overlapping by setting absolute position to 0
            --   -- , margin $  10 
            --   ] [
            --     if item.isEditable then 
            --       inputTextField push item config index
            --     else 
            --       nonEditableTextView push item config index
            --   ]
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
                  Just item -> getVerticalMargins item.inputTextConfig.margin
    offset = textHeight / 2
    marginTop = marginVertical + offset
    maxHeight = spy "maxHeight" $ (((spy "textHeight" textHeight) + (spy "marginVertical" marginVertical)) * (spy "len" len)) 
    bottomPadding = (offset + marginVertical)
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , margin $ MarginVertical (marginTop) bottomPadding
      , orientation VERTICAL
      ][(SeparatorView.view (separatorConfig { count = 10 * len, layoutHeight = V $ maxHeight - bottomPadding - marginTop}))]  -- initially 10 * len

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
            ] <> FontStyle.subHeading2 LanguageStyle
        , imageView
            [ imageWithFallback $ fetchImage FF_ASSET config.suffixButton.suffixImage
            , height $ V 16
            , width $ V 16
            , margin $ MarginTop 1
            ]
        ]
    ]


nonEditableTextView :: forall w. (Action -> Effect Unit ) -> InputView -> InputViewConfig -> Int -> PrestoDOM (Effect Unit) w
nonEditableTextView push config config' index = let
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
    , onClick push $ const $ TextFieldFocusChanged config.inputTextConfig.id true true
    , rippleColor $ Color.rippleShade
    , stroke $ strokeValue 
    -- , background Color.red900
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


inputTextField :: forall w. (Action -> Effect Unit ) -> InputView -> InputViewConfig -> Int -> PrestoDOM (Effect Unit) w
inputTextField push config config' index =
  let _ = spy  "InputView" config
      marginTop = 48 * index
  in 
  -- relativeLayout[
  --   height WRAP_CONTENT
  -- , width MATCH_PARENT
  -- ][
    linearLayout[
    height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , gravity CENTER
  , margin $ MarginTop marginTop
  -- , background Color.white900
  ][ linearLayout[
      height WRAP_CONTENT
    , weight 1.0
    , orientation HORIZONTAL
  ]
  [
    prefixDotImageView push config (length config'.inputView)
  , 
  frameLayout[
      height WRAP_CONTENT
    ,
     width MATCH_PARENT 
    ][
  linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    -- , margin config.inputTextConfig.margin 
    -- , margin $ MarginTop 12
    ][
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , background Color.squidInkBlue
      , margin $ MarginTop 15
      , cornerRadius $ config.inputTextConfig.cornerRadius 
      -- , orientation VERTICAL
      ][  editText $ 
          [ height $ config.height 
          , weight 1.0
          , background Color.squidInkBlue
          , singleLine true
          , ellipsize true
          , pattern "[^\n]*,255"
          , padding $ config.padding 
          , color config.inputTextConfig.textColor
          , gravity LEFT
          , cornerRadius $ config.inputTextConfig.cornerRadius 
          -- , text config.inputTextConfig.textValue
          , selectAllOnFocus true
          , singleLine true
          , hint config.inputTextConfig.hint
          , hintColor $ Color.blueGrey
          , pattern "[^\n]*,255"
          -- , margin $ MarginLeft 5  
          , id $ (spy ("edit text id "<> show index) (getNewIDWithTag $ config.inputTextConfig.id )) 
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
                let _ = spy "action" action 
                case action of 
                  TextFieldFocusChanged _ _ hasFocus -> do 
                    if (isTrue hasFocus) then do 
                      push action
                      else  
                        pure unit
                  _ -> pure unit
              ) $ TextFieldFocusChanged config.inputTextConfig.id false index--config.index
          , cursorColor Color.yellow900
          ] <> FontStyle.subHeading1 TypoGraphy
        -- , crossButtonView push config
      ]
    , bottomStrokeView $ boolToVisibility config.inputTextConfig.isFocussed
      ],
      linearLayout[
        height WRAP_CONTENT
      , width MATCH_PARENT
      ,  gravity RIGHT

      , margin $ Margin 0 0 15 0
      -- , background Color.green900
      ]
    (
      if config.index /= 0 && config.index /=1 && config.index /= length config'.inputView && length config'.inputView > 2 --length config'.inputView > 2 
      then [swapButtonView push config]
      else []
    )
      ]]
    , postfixImageView push config (length config'.inputView)
    ]
  -- ]

swapButtonView :: forall w. (Action -> Effect Unit) -> InputView -> PrestoDOM (Effect Unit) w
swapButtonView push config =
      linearLayout[
      width WRAP_CONTENT,
      height WRAP_CONTENT
      -- margin $ MarginTop 15,
      -- background Color.red900
      , layoutGravity "bottom"
      -- , background Color.red
      -- , alignParentBottom "true,-1"

    ][
      linearLayout 
      [ width config.inputTextConfig.swapImageConfig.layoutWidth 
      , height config.inputTextConfig.swapImageConfig.layoutHeight
      , cornerRadius config.inputTextConfig.swapImageConfig.layoutCornerRadius
      , gravity CENTER
      -- , padding config.inputTextConfig.swapImageConfig.layoutPadding
      -- , margin config.inputTextConfig.swapImageConfig.layoutMargin 
      , background Color.black900--config.inputTextConfig.swapImageConfig.layoutColor
      -- , onClick push $ const $ (determineAction config len) 

      ][ imageView
        [ height config.inputTextConfig.swapImageConfig.height 
        , width config.inputTextConfig.swapImageConfig.width 
        , imageWithFallback $ fetchImage FF_COMMON_ASSET config.inputTextConfig.swapImageConfig.imageName 
        ]
      --   , textView
      --       [ text $ show config.index
      -- , color Color.white900
      --                        , textSize FontSize.a_14
      --                       --  , fontStyle $ FontStyle.regular LanguageStyle
      --       ]
      ]
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

postfixImageView :: forall w. (Action -> Effect Unit ) -> InputView -> Int -> PrestoDOM (Effect Unit) w
postfixImageView push config len = 
  linearLayout[
    width WRAP_CONTENT,
    height MATCH_PARENT,
    margin $ MarginTop 12
  ][
    linearLayout 
    [ width config.inputTextConfig.postfixImageConfig.layoutWidth 
    , height config.inputTextConfig.postfixImageConfig.layoutHeight
    , cornerRadius config.inputTextConfig.postfixImageConfig.layoutCornerRadius
    , gravity CENTER
    , padding config.inputTextConfig.postfixImageConfig.layoutPadding
    , margin config.inputTextConfig.postfixImageConfig.layoutMargin 
    , background config.inputTextConfig.postfixImageConfig.layoutColor
    , onClick push $ const $ (determineAction config len) 

    ][ imageView
      [ height config.inputTextConfig.postfixImageConfig.height 
      , width config.inputTextConfig.postfixImageConfig.width 
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
    , margin  $ MarginTop 10
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

-- inputImageView :: forall w. (Action -> Effect Unit ) -> InputView -> InputViewConfig -> Int -> PrestoDOM (Effect Unit) w
-- inputImageView push config config' index= let 
--   lastIndex = (length config'.inputView) - 1
--   in
--   linearLayout
--               [ height WRAP_CONTENT
--               , width WRAP_CONTENT
--               , orientation VERTICAL 
--               , gravity CENTER
--               ][ (SeparatorView.view (config.imageSeparator)) 
--               ]   (commenting mine)

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
