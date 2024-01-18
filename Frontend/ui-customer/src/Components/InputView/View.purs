module Components.InputView.View where

import Prelude
import Effect (Effect)
import Components.InputView.Controller
import Components.SeparatorView.View as SeparatorView
import Styles.Colors as Color
import Mobility.Prelude (boolToVisibility)
import PrestoDOM (PrestoDOM(..), Orientation(..), Length(..), Visibility(..), Gravity(..), Padding(..), Margin(..), linearLayout, height, width, orientation, margin, padding, textView, color, background, cornerRadius, weight, text, imageView, imageWithFallback, stroke, gravity, visibility, onChange, onFocus, onClick, selectAllOnFocus, hint, hintColor, cursorColor, pattern, maxLines, singleLine, ellipsize, editText, id)
import Data.Array (mapWithIndex, length)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Engineering.Helpers.Commons (getNewIDWithTag)
import Font.Style as FontStyle
import Common.Types.App (LazyCheck(..))
import JBridge (debounceFunction)
import Resources.Constants (getDelayForAutoComplete)

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
      ][  inputImageView push state
        , textViews push state
      ]
    ]

backPressView :: forall w. InputViewConfig -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
backPressView config push = 
  linearLayout
  [ height MATCH_PARENT
  , width WRAP_CONTENT
  , padding $ config.backIcon.padding 
  , onClick push $ const BackPressed
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
    ][]
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


inputImageView :: forall w. (Action -> Effect Unit ) -> InputViewConfig -> PrestoDOM (Effect Unit) w
inputImageView push config = 
  let lastIndex = (length config.inputView) - 1
  in
  linearLayout
    [ height WRAP_CONTENT
    , width $ config.imageLayoutWidth 
    , orientation VERTICAL
    , margin $ config.imageLayoutMargin
    , gravity CENTER
    , visibility config.imageLayoutVisibility
    ] 
    (mapWithIndex (\index item -> prefixImageView push (index /= lastIndex) item ) config.inputView)
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
    , onClick push $ const $ TextFieldFocusChanged config.id true
    , stroke $ strokeValue 
    ]
    [  textView $ 
          [ text $ config.textValue
          , color config.textColor
          , width MATCH_PARENT
          , height WRAP_CONTENT
          , maxLines 1
          , ellipsize true
          , gravity config.gravity
          ] <> config.fontStyle -- (FontStyle.body6 LanguageStyle)
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
          , onFocus push $ const $ TextFieldFocusChanged config.id true
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

emptyTextView :: forall w. PrestoDOM (Effect Unit) w
emptyTextView = textView[height $ V 0, visibility GONE]