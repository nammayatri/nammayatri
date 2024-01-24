module Components.InputView.View where

import Components.InputView.Controller

import Common.Types.App (LazyCheck(..))
import Components.SeparatorView.View as SeparatorView
import Data.Array (mapWithIndex, length)
import Effect (Effect)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Font.Style as FontStyle
import Helpers.CommonView (emptyTextView)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import JBridge (debounceFunction)
import Mobility.Prelude (boolToVisibility)
import Prelude (Unit, const, discard, void, ($), (-), (/=), (<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, color, cornerRadius, cursorColor, editText, ellipsize, gravity, height, hint, hintColor, id, imageView, imageWithFallback, linearLayout, margin, maxLines, onChange, onClick, onFocus, orientation, padding, pattern, selectAllOnFocus, singleLine, stroke, text, textView, visibility, weight, width)
import Resources.Constants (getDelayForAutoComplete)
import Styles.Colors as Color

view :: forall w. (Action -> Effect Unit) -> InputViewConfig -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ PaddingHorizontal 16 16
    , background Color.black900
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        ]
        [ backPressView state push
        , dateTimePickerButton state push
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ PaddingVertical 16 16
        , gravity CENTER_VERTICAL
        ]
        [ inputImageView push state
        , inputLayoutView push state
        ]
    ]

backPressView :: forall w. InputViewConfig -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
backPressView config push =
  linearLayout
    [ height MATCH_PARENT
    , width WRAP_CONTENT
    , padding $ config.backIcon.padding
    , onClick push $ const BackPressed
    ]
    [ imageView
        [ height $ config.backIcon.height
        , width $ config.backIcon.width
        , imageWithFallback config.backIcon.imageName
        ]
    , textView $
        [ text $ config.headerText
        , visibility $ boolToVisibility $ config.headerVisibility
        , color Color.white900
        , margin $ MarginLeft 8
        ] <> FontStyle.subHeading2 LanguageStyle
    ]

dateTimePickerButton :: forall w. InputViewConfig -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
dateTimePickerButton config push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , margin $ MarginTop 16
    , visibility config.suffixButton.visibility
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , weight 1.0
        ]
        []
    , linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , background Color.squidInkBlue
        , cornerRadius 4.0
        , padding $ config.suffixButton.padding
        , gravity config.suffixButton.gravity
        , onClick push $ const DateTimePickerButtonClicked
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
          ]
      , if showSeparator then SeparatorView.view (config.imageSeparator) else emptyTextView
      ]

inputLayoutView :: forall w. (Action -> Effect Unit) -> InputViewConfig -> PrestoDOM (Effect Unit) w
inputLayoutView push config =
  linearLayout
    [ height WRAP_CONTENT
    , weight 1.0
    , padding $ config.inputLayoutPading
    , orientation VERTICAL
    ]
    ( mapWithIndex
        ( \index item ->
            if item.isEditable then inputTextField push item
            else nonEditableTextView push item
        )
        config.inputView
    )

nonEditableTextView :: forall w. (Action -> Effect Unit) -> InputView -> PrestoDOM (Effect Unit) w
nonEditableTextView push config =
  let
    strokeValue = ((if config.inputTextConfig.isFocussed then "1," else "0,") <> Color.yellow900)
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , gravity CENTER
      , padding $ config.padding
      , background Color.squidInkBlue
      , margin $ config.inputTextConfig.margin
      , cornerRadius $ config.inputTextConfig.cornerRadius
      , onClick push $ const $ TextFieldFocusChanged config.inputTextConfig.id true
      , stroke $ strokeValue
      ]
      [ textView $
          [ text $ config.inputTextConfig.textValue
          , color config.inputTextConfig.textColor
          , width MATCH_PARENT
          , height WRAP_CONTENT
          , maxLines 1
          , ellipsize true
          , gravity config.gravity
          ] <> config.fontStyle
      ]

inputTextField :: forall w. (Action -> Effect Unit) -> InputView -> PrestoDOM (Effect Unit) w
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
        ]
        [ editText $
            [ height $ config.height
            , weight 1.0
            , padding $ config.padding
            , color Color.black600
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
            , onChange
                ( \action -> do
                    void $ debounceFunction getDelayForAutoComplete push AutoCompleteCallBack config.inputTextConfig.isFocussed
                    void $ push action
                )
                InputChanged
            , onFocus push $ const $ TextFieldFocusChanged config.inputTextConfig.id true
            , cursorColor Color.yellow900
            ] <> FontStyle.body6 LanguageStyle
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
    ]
    [ imageView
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
    ]
    []