{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
        ][ textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text state.title
          , color Color.greyTextColor
          , alpha 0.8
          , margin (MarginBottom 10)
          , visibility (if state.title == (getString MOBILE_NUMBER ) then GONE else VISIBLE)
          ] <> FontStyle.paragraphText TypoGraphy
          , textView $
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text "  *"
            , color Color.redRoman
            , alpha 0.8
            , margin (MarginBottom 10)
            , visibility (if state.title == (getString MOBILE_NUMBER ) || state.valueId == "EditTextOtp" then GONE else VISIBLE)
            ] <> FontStyle.h2 TypoGraphy
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , stroke if state.isinValid then ("1," <> Color.lightMaroon) else  ("1," <> state.stroke)
          , background state.background
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
              , onFocus push (const TextClicked)
              , id state.id
              ] <> FontStyle.subHeading1 TypoGraphy
              )
            ]
        , PrestoAnim.animationSet [fadeIn state.isinValid, fadeOut $ not state.isinValid]
          $ textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text (fromMaybe "" state.error)
          , visibility if state.isinValid then VISIBLE else GONE
          , color Color.lightMaroon
          , margin (MarginTop 10)
          ] <> FontStyle.paragraphText TypoGraphy
    ]