{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.InAppKeyboardModal.View where

import Common.Types.App
import Components.InAppKeyboardModal.Controller (Action(..), InAppKeyboardModalState, SingleElementTextBoxConfig, InputFieldConfig)
import Animation (translateYAnim)
import Animation.Config (translateYAnimConfig)
import Data.Array (mapWithIndex)
import Data.String (take, drop, length)
import Effect (Effect)
import Engineering.Helpers.Commons (screenWidth)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Types (STR(..))
import Prelude (Unit, const, map, unit, ($), (/), (<>), (==), (||), (>=), (&&), (<), not, (<$>), (/=))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), LetterSpacing(..) , imageUrl, imageView, linearLayout, onBackPressed, onClick, textView, alpha)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (background, backgroundDrawable, clickable, color, cornerRadii, cornerRadius, fontStyle, gravity, height, imageUrl, margin, orientation, padding, stroke, text, textSize, weight, width, visibility,imageWithFallback, letterSpacing)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Styles.Colors as Color
import Screens.Types(KeyboardModalType(..)) as KeyboardModalType
import Language.Strings (getString)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))
import Debug (spy)
import Data.Array as DA

view :: forall w . (Action -> Effect Unit) -> InAppKeyboardModalState -> PrestoDOM (Effect Unit) w
view push state =
    linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , clickable true
    , background Color.black9000
    , gravity BOTTOM
    ][  PrestoAnim.animationSet [ translateYAnim translateYAnimConfig ] $
        linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , cornerRadius 20.0
        , orientation VERTICAL
        , background Color.white900
        , gravity CENTER
        ][  linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , gravity CENTER
            , margin (MarginTop 10)
            ][  linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation HORIZONTAL
                , margin $ if state.isDismissable then (MarginVertical 20 20 ) else (Margin 20 20 20 20 )
                , gravity CENTER_VERTICAL
                ][  imageView
                    [ width (V 35)
                    , height (V 35)
                    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
                    , onClick push (const BackPressed)
                    , visibility if state.isDismissable then VISIBLE else GONE
                    , padding (Padding 5 5 5 5)
                    ]
                  , textView $ 
                    [ width state.headingConfig.width
                    , height state.headingConfig.height
                    , gravity state.headingConfig.gravity
                    , text state.headingConfig.text
                    , color state.headingConfig.color
                    , margin state.headingConfig.margin
                    , visibility state.headingConfig.visibility
                    , cornerRadius state.headingConfig.cornerRadius
                    , padding state.headingConfig.padding
                    , weight state.headingConfig.weight
                    ]  <> (FontStyle.getFontStyle state.inputTextConfig.textStyle LanguageStyle)

                ]
              , otpView push state
              ]
          , keyboard push state
          ]
      ]

textBoxes :: forall w . (Action -> Effect Unit) -> InAppKeyboardModalState -> SingleElementTextBoxConfig -> PrestoDOM (Effect Unit) w
textBoxes push state textBoxConfig =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  , visibility if (state.modalType == KeyboardModalType.OTP && not state.otpAttemptsExceeded) || state.modalType == KeyboardModalType.ODOMETER then VISIBLE else GONE
  , margin (MarginVertical 20 20)
  , clickable false
  ](mapWithIndex (\index item ->
      textView $
      [ width state.textBoxConfig.width 
      , height state.textBoxConfig.height
      , color if ( take 1 (drop index state.inputTextConfig.text) == "•" ) then Color.black600 else Color.greyTextColor
      , text if ( take 1 (drop index state.inputTextConfig.text) ) == "-" then "" else ( take 1 (drop index state.inputTextConfig.text) ) 
      , gravity CENTER
      , cornerRadius 4.0
      , stroke ("1," <> if (state.otpIncorrect || state.otpAttemptsExceeded ) then Color.textDanger else if state.inputTextConfig.focusIndex == index then Color.highlightBorderColor else Color.borderColorLight )
      , margin textBoxConfig.margin
      , onClick push (const (OnclickTextBox index))
      ]<> (FontStyle.getFontStyle state.inputTextConfig.textStyle LanguageStyle)) state.textBoxConfig.textBoxesArray)

singleTextBox :: forall w . (Action -> Effect Unit) -> InAppKeyboardModalState -> PrestoDOM (Effect Unit) w
singleTextBox push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  , cornerRadius 4.0
  , visibility if state.modalType == KeyboardModalType.MOBILE__NUMBER then VISIBLE else GONE
  , clickable false
  , padding (Padding 16 16 16 16)
  , stroke ("1," <> if not state.isValidAlternateNumber then Color.textDanger else Color.borderColorLight )
  ][textView $
      [ width state.inputTextConfig.width
      , height state.inputTextConfig.height
      , color state.inputTextConfig.color
      , text state.inputTextConfig.text
      , weight state.inputTextConfig.weight
      , gravity state.inputTextConfig.gravity
      , visibility state.inputTextConfig.visibility
      , cornerRadius state.inputTextConfig.cornerRadius
      , padding state.inputTextConfig.padding
      , margin state.inputTextConfig.margin
      , onClick push (const (OnclickTextBox 0))
      ] <> (FontStyle.getFontStyle state.inputTextConfig.textStyle LanguageStyle),
    imageView
        [ width $ V 23
         , height $ V 23
         , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_close"
         , visibility if (state.inputTextConfig.text == (getString ENTER_MOBILE_NUMBER)) then GONE else VISIBLE
         , onClick push (const (OnClickTextCross))
        ]
      ]

otpView :: forall w . (Action -> Effect Unit) -> InAppKeyboardModalState -> PrestoDOM (Effect Unit) w
otpView push state =
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , gravity if(state.modalType == KeyboardModalType.MOBILE__NUMBER) then LEFT else CENTER
    ]([ if state.modalType == KeyboardModalType.OTP then textBoxes push state $ otpTextBoxConfig state
        else if state.modalType == KeyboardModalType.ODOMETER then captureOdometerReadingView push state
        else textView[]
      ] <>  [ singleTextBox push state  ] 
        <>  [ textView $
              [ width state.subHeadingConfig.width
              , height state.subHeadingConfig.height
              , color state.subHeadingConfig.color
              , text state.subHeadingConfig.text
              , visibility state.subHeadingConfig.visibility
              , gravity state.subHeadingConfig.gravity
              , cornerRadius state.subHeadingConfig.cornerRadius
              , padding state.subHeadingConfig.padding
              , margin state.subHeadingConfig.margin
              , weight state.subHeadingConfig.weight
              ] <> (FontStyle.getFontStyle state.subHeadingConfig.textStyle LanguageStyle)
            ] 
        <>  [ textView $
              [ width state.errorConfig.width
              , height state.errorConfig.width
              , visibility state.errorConfig.visibility
              , margin state.errorConfig.margin
              , text state.errorConfig.text
              , color state.errorConfig.color
              , gravity state.errorConfig.gravity
              , cornerRadius state.errorConfig.cornerRadius
              , padding state.errorConfig.padding
              , weight state.errorConfig.weight
              ] <> (FontStyle.getFontStyle state.errorConfig.textStyle LanguageStyle)
            ] 
        <>  [ textView $
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text (getString RESEND_OTP)
              , color Color.blue900
              , margin (Margin 0 0 0 0)
              , onClick push (const (OnClickResendOtp))
              , visibility if (state.modalType == KeyboardModalType.OTP && state.showResendOtpButton && (not state.otpAttemptsExceeded)) then VISIBLE else GONE
              ] <> FontStyle.tags TypoGraphy
            ]   
          )          


otpTextBoxConfig :: forall w. InAppKeyboardModalState -> SingleElementTextBoxConfig
otpTextBoxConfig _ = {numberOfBoxes : 4 , width : V 48, height : V 56, margin : (MarginHorizontal 24 12)}


keyboard :: forall w . (Action -> Effect Unit) -> InAppKeyboardModalState -> PrestoDOM (Effect Unit) w
keyboard push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin $ MarginTop 40
  , padding (Padding 0 5 0 20)
  , gravity CENTER
  , background Color.grey800
  ] (map (\(item) ->
    linearLayout
     [ width MATCH_PARENT
     , height WRAP_CONTENT
     , orientation HORIZONTAL
     , margin (Margin 4 0 4 0)
     , gravity CENTER
     ] (mapWithIndex (\index key ->
       linearLayout
       [ width MATCH_PARENT
       , height WRAP_CONTENT
       , gravity CENTER
       , weight 1.0
       , backgroundDrawable "button"
       ][  if (key == "back" || key == "done") then
           linearLayout
           [ width MATCH_PARENT
           , height WRAP_CONTENT
           , gravity CENTER
           , margin (Margin 3 3 3 3)
           , alpha if (key == "done") then state.imageConfig.alpha else 1.0
           , background if key == "back" then Color.lightGrey else state.confirmBtnColor
           , cornerRadius 4.0
           , cornerRadii $ if key == "back" then Corners 30.0 false false false true else Corners 30.0 false false true false
           , onClick push if key == "back" then (const (OnClickBack state.inputTextConfig.text)) else (const (OnClickDone state.inputTextConfig.text))
           , clickable if key == "back" then true 
                      else ((length state.inputTextConfig.text == (DA.length state.textBoxConfig.textBoxesArray) && state.modalType == KeyboardModalType.OTP && not state.otpIncorrect ) || (length state.inputTextConfig.text == 10  && state.modalType == KeyboardModalType.MOBILE__NUMBER && state.isValidAlternateNumber)) || state.modalType == KeyboardModalType.ODOMETER
           ][ 
                if key == "back" then 
                imageView
                  [ width $ V 24
                  , height $ V 24
                  , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_delete"
                  , margin (Margin 0 18 0 18)
                  ]
                else
                  imageView
                  [ width $ V 24
                  , height $ V 24
                  , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_tick_white"
                  , margin (Margin 0 18 0 18)
                  ]
           ]
           else
           linearLayout
           [ width MATCH_PARENT
           , height WRAP_CONTENT
           , gravity CENTER
           , margin (Margin 3 3 3 3)
           , background Color.white900
           , cornerRadius 4.0
           , onClick push (const (OnSelection key state.inputTextConfig.focusIndex))
           ][  textView $
               [ width WRAP_CONTENT
               , height MATCH_PARENT
               , text key
               , color Color.greyTextColor
               , padding (Padding 0 15 0 15)
               ] <> FontStyle.h1 TypoGraphy
           ]
       ]
       ) item.keys )
    ) state.keyList )


captureOdometerReadingView :: forall w . (Action -> Effect Unit) -> InAppKeyboardModalState -> PrestoDOM (Effect Unit) w
captureOdometerReadingView push config = 
  linearLayout
  [ height WRAP_CONTENT
  , gravity CENTER
  , margin $ MarginHorizontal 16 16
  , width MATCH_PARENT
  ][  inputField push config {  isAdjustable : true,
                                width : (V 0),
                                isActive : config.odometerConfig.updateKm,
                                unitVal : "Km",
                                letterSpacing : (6.0),
                                textVal : config.odometerReading.kiloMeters
                              }
    , textView  
      [ text "•"
      , height WRAP_CONTENT
      , width WRAP_CONTENT
      , margin $ MarginHorizontal 16 16
      , textSize FontSize.a_14 
      , fontStyle $ FontStyle.bold LanguageStyle
      ]
    , inputField push config {  isAdjustable : false,
                                width : (V 110),
                                isActive : config.odometerConfig.updateM,
                                unitVal : "m",
                                letterSpacing : (2.0),
                                textVal : config.odometerReading.meters
                              }
  ]

inputField :: forall w . (Action -> Effect Unit) -> InAppKeyboardModalState -> InputFieldConfig -> PrestoDOM (Effect Unit) w
inputField push config inputFieldConfig = 
  linearLayout
  ([ height $ V 60
  , cornerRadius 8.0 
  , stroke $ "1,"<> if inputFieldConfig.isActive then Color.blue900 else Color.grey800
  , onClick push $ const $ OnTextViewClick (inputFieldConfig.unitVal)
  , padding $ Padding 16 16 16 16
  ] <> if inputFieldConfig.isAdjustable then [weight 1.0] else [width inputFieldConfig.width])[  textView $
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , text inputFieldConfig.textVal
      , letterSpacing $ PX inputFieldConfig.letterSpacing
      , height MATCH_PARENT
      , color Color.black900
      ] <> FontStyle.h2 TypoGraphy
    , textView $
      [ height MATCH_PARENT
      , width WRAP_CONTENT
      , text inputFieldConfig.unitVal
      , margin $ MarginLeft 12
      , color Color.black600
      ] <> FontStyle.h2 TypoGraphy
  ]

