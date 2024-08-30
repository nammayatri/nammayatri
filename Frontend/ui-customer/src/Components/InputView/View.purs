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
import PrestoDOM (PrestoDOM(..), Orientation(..), Length(..), Visibility(..), Gravity(..), Padding(..), Margin(..), Accessiblity(..), linearLayout, height, width, orientation, margin, padding, textView, color, background, cornerRadius, weight, text, imageView, imageWithFallback, stroke, gravity, visibility, onChange, onFocus, onClick, selectAllOnFocus, hint, hintColor, cursorColor, pattern, maxLines, singleLine, ellipsize, editText, id, clickable, afterRender, textSize, rippleColor, layoutGravity, relativeLayout, frameLayout, alignParentBottom, root, accessibility, accessibilityHint, setCursorAtEnd, focus)
import Data.Array (mapWithIndex, length, head, any, notElem)
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
import PrestoDOM.Elements.Keyed as Keyed
import Data.Tuple (Tuple(..))
import Screens.Types

view :: forall w. (Action -> Effect Unit) -> InputViewConfig -> PrestoDOM (Effect Unit) w
view push state = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation if state.headerText /= "" then VERTICAL else HORIZONTAL
    , padding $ PaddingHorizontal 16 16
    , background Color.black900 
    ][  if state.headerVisibility then backPressView state push else emptyTextView
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ PaddingVertical 0 16
        , gravity CENTER_VERTICAL
        -- , background Color.red900
        ][  if not state.headerVisibility then backPressView state push else emptyTextView
          -- , if state.imageLayoutVisibility == VISIBLE then inputImageView push state else emptyTextView
          , inputLayoutViews push state]
        ]
backPressView :: forall w. InputViewConfig -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
backPressView config push = 
  linearLayout
    [ height MATCH_PARENT
    , width WRAP_CONTENT-- $ if config.headerVisibility then MATCH_PARENT else WRAP_CONTENT
    , padding $ config.backIcon.padding 
    -- , background Color.green900
    , margin $ MarginTop 10
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
          , imageWithFallback $ fetchImage FF_COMMON_ASSET (spy "prefixImage" config.prefixImage.imageName)
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
  -- , background Color.red900
  ][linearLayout
    [ height WRAP_CONTENT
    , orientation VERTICAL
    ][inputImageView2 push config]
    , relativeLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
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
    val = if config.headerText /= "" then 15 else 5
    textHeight = case head config.inputView of 
                  Nothing -> 37
                  Just item -> getHeight item.height
    marginVertical = case head config.inputView of 
                  Nothing -> 17
                  Just item -> getVerticalMargins item.inputTextConfig.margin
    offset = (textHeight / 2) + 6
    marginTop = offset + val
    bottomPadding = offset
    actualHeight = textHeight + val
    maxHeight = spy "maxHeight" $ (actualHeight * len) - ((actualHeight - 48) *(len - 1) )- bottomPadding - marginTop
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , margin $ MarginVertical (marginTop) 0
      , orientation VERTICAL
      ][(SeparatorView.view (separatorConfig { count = 10 * len, layoutHeight = V $ maxHeight }))]  

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
  -- strokeValue = ((if config.inputTextConfig.isFocussed then "1," else "0,") <> Color.yellow900)
  marginTop = 48 * index
  in 
    linearLayout[
    height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , gravity CENTER
  , margin $ MarginTop marginTop
  ][linearLayout[
      height WRAP_CONTENT
    , weight 1.0
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
  ]
  [
  frameLayout[
      height WRAP_CONTENT
    , width MATCH_PARENT 
    ][
  linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ if config'.headerText == "" then MarginTop 5 else MarginTop 15
    ][ prefixDotImageView push config (length config'.inputView)
  , linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ][
  linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity LEFT--config.gravity
    , padding $ config.padding 
    , background config'.backgroundColor--Color.squidInkBlue
    -- , margin $ config.inputTextConfig.margin
    , cornerRadius $ config.inputTextConfig.cornerRadius 
    , clickable $ config.isClickable 
    , onClick push $ const $ TextFieldFocusChanged config.inputTextConfig.id true config.index true
    , rippleColor $ Color.rippleShade
    -- , stroke $ strokeValue 
    -- , background Color.red900
    ]
    [  
      -- imageView
      --   [ imageWithFallback $ config.inputTextConfig.prefixImageConfig.imageName
      --   , height $ config.inputTextConfig.prefixImageConfig.height
      --   , width $ config.inputTextConfig.prefixImageConfig.width
      --   , visibility $ config.inputTextConfig.prefixImageVisibility
      --   ],
       textView $ 
          [ text $ if config.inputTextConfig.textValue == "" then config.inputTextConfig.placeHolder else config.inputTextConfig.textValue --config.place
          , color config.inputTextConfig.textColor
              , height WRAP_CONTENT
              , maxLines 1
              , ellipsize true
          ] <> config.fontStyle]]]
  ]]
  -- , postfixImageView push config (length config'.inputView)
  ]

inputTextField :: forall w. (Action -> Effect Unit ) -> InputView -> InputViewConfig -> Int -> PrestoDOM (Effect Unit) w
inputTextField push config config' index =
  let _ = spy  "InputView -> " config.index
      _ = spy "config'" config'
      marginTop = (48 * index) - (if config'.headerText == "" && index /= 0 then 10 else 0)
  in 
    linearLayout[
    height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , gravity CENTER
  , margin $ MarginTop  marginTop
  -- , background Color.red900
  ][ linearLayout[
      height WRAP_CONTENT
    , weight 1.0
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
  ]
  [
  frameLayout[
      height WRAP_CONTENT
    , width MATCH_PARENT 
    -- , background Color.green900
    ][
  linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ spy "marginTop in" $ if config'.headerText == "" && index == 0 then MarginTop 5 else MarginTop 15
    ][ prefixDotImageView push config (length config'.inputView)
  , linearLayout 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ][
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , background Color.squidInkBlue
      , cornerRadius $ config.inputTextConfig.cornerRadius 
      ][ editText $ 
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
          , selectAllOnFocus true
          , singleLine true
          , hint config.inputTextConfig.hint
          , hintColor $ Color.blueGrey
          , pattern "[^\n]*,255"
          , setCursorAtEnd true
          , id $ getNewIDWithTag config.inputTextConfig.id 
          , text config.place
          , focus config.inputTextConfig.isFocussed
          , onChange (\action -> do 
              case action of 
                InputChanged _ text -> if text /= "false" && text /= config.inputTextConfig.textValue then do --config.place 
                  void $ debounceFunction getDelayForAutoComplete push AutoCompleteCallBack config.inputTextConfig.isFocussed
                  void $ push action
                  else pure unit 
                _ -> push action
              pure unit
            ) $ (InputChanged config.index)      
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
                  TextFieldFocusChanged _ _ _ hasFocus -> do 
                    if (isTrue hasFocus) then do 
                      push action
                      else       
                        pure unit
                  _ -> pure unit
              ) $ TextFieldFocusChanged config.inputTextConfig.id false index--config.index
          , cursorColor Color.yellow900
          ] <> FontStyle.subHeading1 TypoGraphy
        , if config.crossBtnEnabled then crossButtonView push config else linearLayout[][]
      ]
    , bottomStrokeView $ boolToVisibility config.inputTextConfig.isFocussed
      ]],
      linearLayout[
        height WRAP_CONTENT
      , width MATCH_PARENT
      ,  gravity RIGHT
      , margin $ Margin 0 0 15 0
      -- , background Color.green900
      ]( if (notElem config.index [0,1, length config'.inputView] && length config'.inputView > 2) 
      then [swapButtonView push config]
      else []
    )
      ]]
    , postfixImageView push config (length config'.inputView)
    ]

swapButtonView :: forall w. (Action -> Effect Unit) -> InputView -> PrestoDOM (Effect Unit) w
swapButtonView push config =
    let ind1 = config.index
        ind2 = ind1 - 1
    in
      linearLayout[
        width WRAP_CONTENT
      , height WRAP_CONTENT
      , layoutGravity "bottom"
      -- , margin $ MarginBottom 25
    ][
      linearLayout 
      [ width config.inputTextConfig.swapImageConfig.layoutWidth 
      , height config.inputTextConfig.swapImageConfig.layoutHeight
      , cornerRadius config.inputTextConfig.swapImageConfig.layoutCornerRadius
      , gravity CENTER
      , background Color.black900--config.inputTextConfig.swapImageConfig.layoutColor
      , onClick push $ const $ SwapLocation ind1 ind2
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
      else if inputView.index /= 0 
        then AddRemoveStopAction "ADD" inputView.index 
      else NoAction
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