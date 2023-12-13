{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.AadhaarVerificationScreen.View where

import Common.Types.App
import Screens.AadhaarVerificationScreen.ComponentConfig
import Animation as Anim
import Animation.Config as AnimConfig
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText.View as PrimaryEditText
import Data.Maybe (Maybe(..))
import Debug (spy)
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils as HU
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog)
import Prelude (Unit, bind, const, discard, not, pure, unit, ($), (<<<), (<>), (==), (&&), (/=))
import PrestoDOM (Gravity(..), Length(..), LetterSpacing(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alpha, background, clickable, color, cornerRadius, frameLayout, gravity, height, imageUrl, imageView, imageWithFallback, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, stroke, text, textFromHtml, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import Screens.AadhaarVerificationScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (AadhaarStage(..))
import Screens.Types as ST
import Styles.Colors as Color
import ConfigProvider

screen :: ST.AadhaarVerificationScreenState -> Screen Action ST.AadhaarVerificationScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "AadhaarVerificationScreen"
  , globalEvents:
      [ ( \push -> do
            if (initialState.props.currentStage == VerifyAadhaar) && (not initialState.props.resendEnabled) then do
              _ <- pure $ HU.clearTimer ""
              _ <- HU.startTimer 60 true push ResendTimer
              pure unit
            else
              pure unit
            pure (pure unit)
        )
      ]
  , eval:
      ( \action state -> do
          let
            _ = spy "AadhaarVerificationScreenState action" action
          let
            _ = spy "AadhaarVerificationScreenState state" state
          eval action state
      )
  }

view ::
  forall w.
  (Action -> Effect Unit) ->
  ST.AadhaarVerificationScreenState ->
  PrestoDOM (Effect Unit) w
view push state =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ]
    $ [ linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , background Color.white900
          , clickable true
          , afterRender
              ( \_ ->
                  JB.requestKeyboardShow
                    $ case state.props.currentStage of
                        EnterAadhaar -> (EHC.getNewIDWithTag "EnterAadhaarNumberEditText")
                        VerifyAadhaar -> (EHC.getNewIDWithTag "EnterAadhaarOTPEditText")
                        AadhaarDetails -> (EHC.getNewIDWithTag "EnterAadhaarNameEditText")
              )
              (const unit)
          , onBackPressed push (const BackPressed)
          ]
          [ PrestoAnim.animationSet
              [ Anim.fadeIn true
              ]
              $ backArrow state push
          , verificationFailedView state
          , enterAadhaarNumberView push state
          , enterAadhaarOTPView push state
          , enterAadhaarDetailsView push state
          , PrestoAnim.animationSet
              [ Anim.fadeIn true
              ]
              $ linearLayout
                  [ width MATCH_PARENT
                  , weight 1.0
                  , gravity BOTTOM
                  ]
                  [ PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonViewConfig state) ]
          ]
      ]
    <> if state.props.showLogoutPopup then [ PopUpModal.view (push <<< PopUpModalAC) (logOutPopUpModelConfig state) ] else []

--------------------- backArrow ----------------------------
backArrow :: ST.AadhaarVerificationScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
backArrow state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER
    , margin $ MarginBottom if state.props.currentStage == AadhaarDetails then 0 else 20
    , padding (Padding 16 16 16 16)
    ]
    [ imageView
        [ width (V 25)
        , height (V 25)
        , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_back"
        , onClick push (const BackPressed)
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , weight 1.0
        ]
        []
    , textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text $ getString LOGOUT
          , padding $ Padding 5 5 5 5
          , color Color.blue900
          , onClick push $ const $ Logout
          ]
        <> FontStyle.body1 TypoGraphy
    ]

------------------------- enterAadhaarNumberTextView -------------------
enterAadhaarNumberTextView :: ST.AadhaarVerificationScreenState -> forall w. PrestoDOM (Effect Unit) w
enterAadhaarNumberTextView _ =
  textView
    $ [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , text $ getString ENTER_AADHAAR_NUMBER
      , color Color.textPrimary
      ]
    <> FontStyle.h1 TypoGraphy

------------------------- enterAadhaarDetailsTextView -------------------
enterAadhaarDetailsTextView :: ST.AadhaarVerificationScreenState -> forall w. PrestoDOM (Effect Unit) w
enterAadhaarDetailsTextView _ =
  textView
    $ [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , text $ getString ENTER_AADHAAR_DETAILS
      , color Color.textPrimary
      , margin $ MarginBottom 10
      ]
    <> FontStyle.h1 TypoGraphy

------------------------- enterAadhaarOTPTextView -------------------
enterAadhaarOTPTextView :: ST.AadhaarVerificationScreenState -> forall w. PrestoDOM (Effect Unit) w
enterAadhaarOTPTextView _ =
  textView
    $ [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , text $ getString ENTER_AADHAAR_OTP_
      , color Color.textPrimary
      , margin $ MarginBottom 32
      ]
    <> FontStyle.h1 TypoGraphy

------------------------- enterAadhaarNumberView -------------------
enterAadhaarNumberView :: (Action -> Effect Unit) -> ST.AadhaarVerificationScreenState -> forall w. PrestoDOM (Effect Unit) w
enterAadhaarNumberView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , visibility if state.props.currentStage == EnterAadhaar then VISIBLE else GONE
    , padding (Padding 16 16 16 16)
    ]
    [ PrestoAnim.animationSet
        [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
        ]
        $ enterAadhaarNumberTextView state
    , PrestoAnim.animationSet
        [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
        ]
        $ PrimaryEditText.view (push <<< AadhaarNumberEditText) (aadhaarNumberEditText state)
    , PrestoAnim.animationSet
        [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
        ]
        $ termsAndConditionsView state
    ]

------------------------- enterAadhaarDetailsView -------------------
enterAadhaarDetailsView :: (Action -> Effect Unit) -> ST.AadhaarVerificationScreenState -> forall w. PrestoDOM (Effect Unit) w
enterAadhaarDetailsView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , visibility if state.props.currentStage == AadhaarDetails then VISIBLE else GONE
    , padding (Padding 16 16 16 16)
    ]
    [ PrestoAnim.animationSet
        [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
        ]
        $ enterAadhaarDetailsTextView state
    , PrestoAnim.animationSet
        [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
        ]
        $ PrimaryEditText.view (push <<< AadhaarNameEditText) (aadhaarNameEditText state)
    , dateOfBirth push state
    , PrestoAnim.animationSet
        [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
        ]
        $ PrimaryEditText.view (push <<< AadhaarGenderEditText) (aadhaarGenderEditText state)
    , textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text $ getString GOTO_YOUR_NEAREST_BOOTH
          , color Color.black900
          ]
        <> FontStyle.body1 TypoGraphy
    ]

verificationFailedView :: ST.AadhaarVerificationScreenState -> forall w. PrestoDOM (Effect Unit) w
verificationFailedView state =
  PrestoAnim.animationSet
    [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
    ]
    $ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , background Color.red
        , padding $ Padding 10 10 10 10
        , gravity CENTER
        , visibility if state.props.currentStage == AadhaarDetails then VISIBLE else GONE
        ]
        [ textView
            $ [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , margin $ MarginLeft 5
              , text $ getString VERIFICATION_FAILED
              , color Color.white900
              ]
            <> FontStyle.body1 TypoGraphy
        ]

dateOfBirth :: (Action -> Effect Unit) -> ST.AadhaarVerificationScreenState -> forall w. PrestoDOM (Effect Unit) w
dateOfBirth push state =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , visibility VISIBLE
    ]
    [ textView
        $ [ text (getString DATE_OF_BIRTH)
          , color Color.greyTextColor
          ]
        <> FontStyle.body3 TypoGraphy
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , margin (MarginVertical 10 10)
        , padding (Padding 20 16 16 16)
        , cornerRadius 4.0
        , stroke ("1," <> Color.borderGreyColor)
        , onClick
            ( \action -> do
                _ <- push action
                JB.datePicker "MINIMUM_EIGHTEEN_YEARS" push $ DatePicker "DATE_OF_BIRTH"
            )
            (const SelectDateOfBirthAction)
        , clickable state.props.isDateClickable
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , orientation HORIZONTAL
            ]
            [ textView
                ( [ text if state.data.driverDob == "" then (getString SELECT_DATE_OF_BIRTH) else state.data.driverDob
                  , color if state.data.driverDob == "" then Color.darkGrey else Color.greyTextColor
                  , weight 1.0
                  , padding (PaddingRight 15)
                  ]
                    <> FontStyle.subHeading1 TypoGraphy
                )
            , imageView
                [ width (V 20)
                , height (V 20)
                , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_calendar"
                ]
            ]
        ]
    ]

------------------------- enterAadhaarOTPView -------------------
enterAadhaarOTPView :: (Action -> Effect Unit) -> ST.AadhaarVerificationScreenState -> forall w. PrestoDOM (Effect Unit) w
enterAadhaarOTPView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , visibility if state.props.currentStage == VerifyAadhaar then VISIBLE else GONE
    , padding (Padding 16 16 16 16)
    ]
    [ PrestoAnim.animationSet
        [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
        ]
        $ enterAadhaarOTPTextView state
    , PrestoAnim.animationSet
        [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
        ]
        $ PrimaryEditText.view (push <<< AadhaarOtpEditText) (aadhaarOTPEditText state)
    , PrestoAnim.animationSet
        [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
        ]
        $ resendOTPView push state
    ]

-------------------------------- termsAndConditionsView ------------------
termsAndConditionsView :: ST.AadhaarVerificationScreenState -> forall w. PrestoDOM (Effect Unit) w
termsAndConditionsView _ =
  let
    config = getAppConfig appConfig
  in
    linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , margin $ MarginTop 10
      ]
      [ linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          ]
          [ textView
              $ [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , text $ getString BY_CLICKING_THIS_YOU_WILL_BE_AGREEING_TO_OUR_TC
                , color Color.greyTextColor
                , alpha 0.5
                ]
              <> FontStyle.body3 TypoGraphy
          , textView
              $ [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , textFromHtml $ "<u>" <> (getString TERMS_AND_CONDITIONS_SHORT) <> "</u>"
                , color Color.primaryBlue
                , onClick (\_ -> JB.openUrlInApp $ config.termsLink) (const unit)
                ]
              <> FontStyle.body3 TypoGraphy
          , textView
              $ [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , text $ getString TC_TAIL
                , color Color.greyTextColor
                , alpha 0.5
                ]
              <> FontStyle.body3 TypoGraphy
          ]
      ]

resendOTPView :: (Action -> Effect Unit) -> ST.AadhaarVerificationScreenState -> forall w. PrestoDOM (Effect Unit) w
resendOTPView push state =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , margin (MarginTop 18)
    , orientation VERTICAL
    ]
    [ textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text (getString RESEND_OTP)
          , color Color.mainPrimary
          , onClick push (const ResendOTP)
          , visibility if state.props.resendEnabled then VISIBLE else GONE
          ]
        <> FontStyle.tags TypoGraphy
    , textView
        $ [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text $ (getString RESEND_OTP_IN) <> "  " <> state.data.timer
          , visibility if state.props.resendEnabled then GONE else VISIBLE
          , color Color.blue900
          , alpha 0.6
          ]
        <> FontStyle.tags TypoGraphy
    ]
