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
import Components.InAppKeyboardModal.Controller (Action(..), InAppKeyboardModalState)
import Animation (translateYAnim)
import Animation.Config (translateYAnimConfig)
import Data.Array (mapWithIndex)
import Data.String (take, drop, length)
import Effect (Effect)
import Engineering.Helpers.Commons (screenWidth)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Types (STR(..))
import Prelude (Unit, const, map, unit, ($), (/), (<>), (==), (||), (>=), (&&), (<), not)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), imageUrl, imageView, linearLayout, onBackPressed, onClick, textView, alpha)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (background, backgroundDrawable, clickable, color, cornerRadii, cornerRadius, fontStyle, gravity, height, imageUrl, margin, orientation, padding, stroke, text, textSize, weight, width, visibility,imageWithFallback)
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
    ][
     PrestoAnim.animationSet [
        translateYAnim translateYAnimConfig
      ] $
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
                , margin (Margin 20 20 20 20 )
                , gravity CENTER_VERTICAL
                ][  imageView
                    [ width (V 35)
                    , height (V 35)
                    , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
                    , onClick push (const BackPressed)
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

textBoxes :: forall w . (Action -> Effect Unit) -> InAppKeyboardModalState -> PrestoDOM (Effect Unit) w
textBoxes push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , gravity CENTER
  , visibility if state.modalType == KeyboardModalType.OTP && not state.otpAttemptsExceeded then VISIBLE else GONE
  , margin (Margin 0 20 0 20)
  , clickable false
  ](mapWithIndex (\index item ->
      textView $
      [ width state.textBoxConfig.width
      , height state.textBoxConfig.height
      , color Color.greyTextColor
      , text ( take 1 (drop index state.inputTextConfig.text) )
      , gravity CENTER
      , cornerRadius 4.0
      , stroke ("1," <> if (state.otpIncorrect || state.otpAttemptsExceeded ) then Color.textDanger else if state.inputTextConfig.focusIndex == index then Color.highlightBorderColor else Color.borderColorLight )
      , margin (Margin ((screenWidth unit)/30) 0 ((screenWidth unit)/30) 0)
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
           , margin (Margin 20 0 20 0)
           , orientation VERTICAL
           , gravity if(state.modalType == KeyboardModalType.OTP) then CENTER else LEFT
       ]
             ([] <> [textBoxes push state] <> [singleTextBox push state] <>
                    [textView $
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
                    ] <>
                    [textView $
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
                  ] <>
                    [textView(
                      [
                        width WRAP_CONTENT
                      , height WRAP_CONTENT
                      , text (getString RESEND_OTP)
                      , color Color.blue900
                      , margin (Margin 0 0 0 0)
                      , onClick push (const (OnClickResendOtp))
                      , visibility if (state.modalType == KeyboardModalType.OTP && state.showResendOtpButton && (not state.otpAttemptsExceeded)) then VISIBLE else GONE
                      ] <> FontStyle.tags TypoGraphy
                    )])                 

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
           , background if key == "back" then Color.lightGrey else Color.darkMint
           , cornerRadius 4.0
           , cornerRadii $ if key == "back" then Corners 30.0 false false false true else Corners 30.0 false false true false
           , onClick push if key == "back" then (const (OnClickBack state.inputTextConfig.text)) else (const (OnClickDone state.inputTextConfig.text))
           , clickable if key == "back" then true 
                      else ((length state.inputTextConfig.text == (DA.length state.textBoxConfig.textBoxesArray) && state.modalType == KeyboardModalType.OTP && not state.otpIncorrect ) || (length state.inputTextConfig.text == 10  && state.modalType == KeyboardModalType.MOBILE__NUMBER && state.isValidAlternateNumber))
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
