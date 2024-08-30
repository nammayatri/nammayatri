module Components.InputTextView.View where

import Prelude
import Components.InputTextView.Controller (Action(..), InputTextConfig)
import Effect (Effect)
import Prelude (Unit, const, map, unit, void, show, ($), (/), (<>), (==), (||), (>=), (&&), (<), (>), not, pure, (<$>), (/=))
import PrestoDOM.Animation as PrestoAnim
import Animation (fadeIn)
import Styles.Colors as Color
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import PrestoDOM.Properties (lineHeight, cornerRadii)
import Components.PrimaryButton as PrimaryButton
import PrestoDOM.Types.DomAttributes (Gravity(..), Corners(..))
import Font.Style as FontStyle
import Common.Types.App (LazyCheck(..))
import PrestoDOM ( Margin(..), Orientation(..), Padding(..), Visibility(..), Length(..), PrestoDOM, onChange, singleLine, pattern, hint, adjustViewWithKeyboard, relativeLayout, background, clickable, color, cornerRadius, fontStyle, Gravity(..), gravity, height, imageUrl, imageView, linearLayout, margin, orientation, text, textFromHtml, textSize, textView, weight, width, padding, visibility, afterRender, editText, onClick, alignParentBottom, imageWithFallback, stroke, layoutGravity )


view :: forall w . (Action -> Effect Unit) -> InputTextConfig -> PrestoDOM (Effect Unit) w
view push state =
    PrestoAnim.animationSet [ fadeIn true ] $
    relativeLayout
    [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        , clickable true
        , gravity BOTTOM
        , background Color.black9000
    ][  inputTextView push state 
        , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ PaddingBottom 16
        , alignParentBottom "true,-1"
        , adjustViewWithKeyboard "true"
        , background Color.white900
        ][
            PrimaryButton.view (push <<< PrimaryButtonAC ) (state.data.doneButtonConfig)
        ,   PrimaryButton.view (push <<< CancelButtonAC ) (state.data.cancelButtonConfig)
        ]
    ]

inputTextView :: forall w. (Action -> Effect Unit) -> InputTextConfig -> PrestoDOM (Effect Unit) w
inputTextView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , padding $ Padding 16 24 16 57
  , background Color.white900
  , cornerRadii $ Corners 20.0 true true false false
  , clickable true
  , alignParentBottom "true,-1"
  ][  
    linearLayout[
        width MATCH_PARENT
    ,   height WRAP_CONTENT
    ,   margin $ Margin 0 0 0 20
    ][
        imageView
            [ width $ V 28
            , height $ V 28
            , margin $ MarginRight 8
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left_black"
            , onClick push $ const BackPress
            ]
    ,   textView
        $ [ text $ state.data.title
            , width MATCH_PARENT
            , height WRAP_CONTENT
            , color Color.black800
            ]
        <> FontStyle.subHeading1 TypoGraphy
    ]
    ,   editTextView state push
  ]

editTextView :: forall w. InputTextConfig -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
editTextView state push =
  linearLayout
  [ height $ V 94
  , width MATCH_PARENT
  , background Color.grey800
  , cornerRadius 8.0
  , orientation HORIZONTAL
  , margin $ MarginBottom 24
  , padding $ Padding 16 16 16 0
  ][  imageView 
      [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_message_square"
      , height $ V 16 
      , width $ V 16 
      , margin $ MarginRight 9 
      ]                   
    , editText
      $
      [ height MATCH_PARENT
      , width $ WRAP_CONTENT
      , gravity LEFT
      , padding $ Padding 0 0 0 0
      , background Color.grey800
      , color Color.black 
      , hint "Please Write Something"
      , weight 1.0
      , pattern "[^\n]*,255"
      , singleLine false 
      , onChange push FeedbackChanged 
      ] <> FontStyle.body3 LanguageStyle
  ]