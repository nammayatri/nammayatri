module Components.PrimaryEditText.Views where
import Prelude (Unit, const, ($), (==), (||), not, (<>))
import Effect (Effect)
import PrestoDOM (InputType(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), Gravity(..), alpha, color, cornerRadius, editText, fontStyle, height, hint, inputType, inputTypeI, linearLayout, margin, onChange, onFocus, orientation, padding, pattern, stroke, text, textSize, textView, visibility, weight, width, background, gravity, letterSpacing, id)
import Components.PrimaryEditText.Controllers (Action(..))
import Font.Style as FontStyle
import Styles.Colors as Color
import Font.Size as FontSize
import Screens.Types (PrimaryEditTextState)
import Data.Maybe (fromMaybe)
import PrestoDOM.Animation as PrestoAnim
import Animation (fadeIn, fadeOut)
import Language.Strings (getString)
import Language.Types(STR(..))
import PrestoDOM.Properties(cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Common.Types.App

view :: forall w .  (Action  -> Effect Unit) -> PrimaryEditTextState -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ][ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        ][ textView
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text state.title
          , textSize FontSize.a_14
          , color Color.greyTextColor
          , fontStyle $ FontStyle.regular LanguageStyle
          , alpha 0.8
          , margin (MarginBottom 10)
          , visibility (if state.title == (getString MOBILE_NUMBER ) then GONE else VISIBLE)
          ]
          , textView
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text "  *"
            , color Color.redRoman
            , textSize FontSize.a_18
            , fontStyle $ FontStyle.bold LanguageStyle
            , alpha 0.8
            , margin (MarginBottom 10)
            , visibility (if state.title == (getString MOBILE_NUMBER ) || state.valueId == "EditTextOtp" then GONE else VISIBLE)
            ]
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , stroke if state.isinValid then ("1," <> Color.lightMaroon) else  ("1," <> Color.borderColorLight)
          , cornerRadius 4.0
          ][  textView (
              [ width $ V 60
              , height MATCH_PARENT
              , text (getString COUNTRY_CODE_INDIA)
              , background Color.greyBackground
              , gravity CENTER
              , cornerRadii $ Corners 4.0 true false false true
              , visibility GONE
              ] <> FontStyle.subHeading1 TypoGraphy
              )
              , textView
              [ width $ V 20
              , height WRAP_CONTENT
              ]
              , editText (
              [ width MATCH_PARENT
              , height (V 54)
              , padding (Padding 0 10 20 10)
              , color Color.greyTextColor
              , text state.text
              , hint state.hint
              , inputType if state.type == "number" then Numeric else if state.type == "password" then Password else TypeText
              , inputTypeI if state.valueId == "VEHICLE_NUMBER" || state.valueId == "VEHICLE_PERMIT_NUMBER" then 4097 else if state.valueId == "DRIVER_NAME" then 1 else if state.valueId == "MODEL_TYPE" || state.valueId == "COLOR" then 1 else 2
              , weight 1.0
              , cornerRadius 4.0
              , pattern (fromMaybe "[a-z, 0-9, A-Z]" state.pattern )
              , letterSpacing state.letterSpacing
              , onChange push (TextChanged state.valueId)
              , stroke if state.isinValid then ("1," <> Color.white900) else ("1," <> Color.white900)
              , onFocus push (const TextClicked)
              , id state.id
              ] <> FontStyle.subHeading1 TypoGraphy
              )
            ]
        , PrestoAnim.animationSet [fadeIn state.isinValid, fadeOut $ not state.isinValid]
          $ textView
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text (fromMaybe "" state.error)
          , visibility if state.isinValid then VISIBLE else GONE
          , textSize FontSize.a_14
          , color Color.lightMaroon
          , fontStyle $ FontStyle.regular LanguageStyle
          , margin (MarginTop 10)
          ]
    ]