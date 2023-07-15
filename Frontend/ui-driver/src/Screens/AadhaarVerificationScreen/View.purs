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
import MerchantConfig.Utils (getValueFromConfig)
import Prelude (Unit, bind, const, discard, not, pure, unit, ($), (<<<), (<>), (==), (&&))
import PrestoDOM (Gravity(..), Length(..), LetterSpacing(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alpha, background, clickable, color, cornerRadius, frameLayout, gravity, height, imageUrl, imageView, imageWithFallback, linearLayout, margin, onBackPressed, onClick, orientation, padding, relativeLayout, stroke, text, textFromHtml, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import Screens.AadhaarVerificationScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types (AadhaarStage(..))
import Screens.Types as ST
import Styles.Colors as Color

screen :: ST.AadhaarVerificationScreenState -> Screen Action ST.AadhaarVerificationScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "AadhaarVerificationScreen"
  , globalEvents : [ (\push -> do
                      if (initialState.props.currentStage == VerifyAadhaar) && (not initialState.props.resendEnabled)then do
                        _ <- pure $ HU.clearTimer ""
                        _ <- HU.startTimer 60 true push ResendTimer
                        pure unit
                      else
                        pure unit
                      pure (pure unit)) ]
  , eval : (\action state -> do
      let _ = spy "AadhaarVerificationScreenState action" action
      let _ = spy "AadhaarVerificationScreenState state" state
      eval action state)
  }

view
  :: forall w
  . (Action -> Effect Unit)
  -> ST.AadhaarVerificationScreenState
  -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ]$[linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , clickable true
    , afterRender (\_ -> JB.requestKeyboardShow $ case state.props.currentStage of 
          EnterAadhaar -> (EHC.getNewIDWithTag "EnterAadhaarNumberEditText")
          VerifyAadhaar -> (EHC.getNewIDWithTag "EnterAadhaarOTPEditText")
        ) (const unit)
    , onBackPressed push (const BackPressed)
    ][    PrestoAnim.animationSet
          [ Anim.fadeIn true
          ] $ backArrow state push
        , enterAadhaarNumberView push state
        , enterAadhaarOTPView push state
        , PrestoAnim.animationSet
          [ Anim.fadeIn true
          ] $ linearLayout
              [ width MATCH_PARENT
              , weight 1.0
              , gravity BOTTOM
              ][PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonViewConfig state)]
    ]
   ] <> if state.props.showLogoutPopup then [PopUpModal.view (push <<< PopUpModalAC) (logOutPopUpModelConfig state)] else []


--------------------- backArrow ----------------------------
backArrow :: ST.AadhaarVerificationScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
backArrow state push =
 linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , gravity CENTER
  , margin $ MarginBottom 30
  , padding (Padding 16 16 16 16)
  ][ imageView
      [ width ( V 25 )
      , height ( V 25 )
      , imageWithFallback "ny_ic_back,https://assets.juspay.in/nammayatri/images/driver/ny_ic_back.png"
      , onClick push (const BackPressed)
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , weight 1.0
      ][]
    ,  textView $ 
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , text $ getString LOGOUT
      , padding $ Padding 5 5 5 5
      , color Color.blue900
      , onClick push $ const $ Logout
      ] <> FontStyle.body1 TypoGraphy
  ]

------------------------- enterMobileNumberTextView -------------------
enterMobileNumberTextView :: ST.AadhaarVerificationScreenState ->  forall w . PrestoDOM (Effect Unit) w
enterMobileNumberTextView _ =
 textView $ 
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , text $ getString ENTER_AADHAAR_NUMBER
  , color Color.textPrimary
  ] <> FontStyle.h1 TypoGraphy

------------------------- enterAadhaarOTPTextView -------------------
enterAadhaarOTPTextView :: ST.AadhaarVerificationScreenState ->  forall w . PrestoDOM (Effect Unit) w
enterAadhaarOTPTextView _ =
 textView $ 
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , text $ "Enter Aadhaar OTP"
  , color Color.textPrimary
  , margin $ MarginBottom 32
  ] <> FontStyle.h1 TypoGraphy
------------------------- enterAadhaarNumberView -------------------
enterAadhaarNumberView :: (Action -> Effect Unit) -> ST.AadhaarVerificationScreenState ->  forall w . PrestoDOM (Effect Unit) w
enterAadhaarNumberView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , visibility if state.props.currentStage == EnterAadhaar then VISIBLE else GONE
  , padding (Padding 16 16 16 16)
  ][     PrestoAnim.animationSet
          [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
          ] $ enterMobileNumberTextView state
        , PrestoAnim.animationSet
          [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
          ] $ PrimaryEditText.view (push <<< AadhaarNumberEditText) (aadhaarNumberEditText state)
        , PrestoAnim.animationSet
          [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
          ] $ termsAndConditionsView state]

------------------------- enterAadhaarOTPView -------------------
enterAadhaarOTPView :: (Action -> Effect Unit) -> ST.AadhaarVerificationScreenState ->  forall w . PrestoDOM (Effect Unit) w
enterAadhaarOTPView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , visibility if state.props.currentStage == VerifyAadhaar then VISIBLE else GONE
  , padding (Padding 16 16 16 16)
  ][     PrestoAnim.animationSet
          [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
          ] $ enterAadhaarOTPTextView state
        , PrestoAnim.animationSet
          [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
          ] $ PrimaryEditText.view (push <<< AadhaarOtpEditText) (aadhaarOTPEditText state)
        , PrestoAnim.animationSet
          [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
          ] $ resendOTPView push state]

-------------------------------- termsAndConditionsView ------------------
termsAndConditionsView :: ST.AadhaarVerificationScreenState -> forall w . PrestoDOM (Effect Unit) w
termsAndConditionsView _ =
 linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , margin $ MarginTop 10
  ][ linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      ][textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ getString BY_CLICKING_THIS_YOU_WILL_BE_AGREEING_TO_OUR_TC
        , color Color.greyTextColor
        , alpha 0.5
        ] <> FontStyle.body3 TypoGraphy
      , textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , textFromHtml $ "<u>" <> (getString TERMS_AND_CONDITIONS_SHORT) <> "</u>"
        , color Color.primaryBlue
        , onClick (\_ -> JB.openUrlInApp $ getValueFromConfig "DOCUMENT_LINK") (const unit)
        ] <> FontStyle.body3 TypoGraphy
      , textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ getString TC_TAIL
        , color Color.greyTextColor
        , alpha 0.5
        ] <> FontStyle.body3 TypoGraphy
      ]
  ]


resendOTPView :: (Action -> Effect Unit) -> ST.AadhaarVerificationScreenState -> forall w . PrestoDOM (Effect Unit) w
resendOTPView push state =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , margin (MarginTop 18)
  , orientation VERTICAL
  ][  textView $
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , text (getString RESEND_OTP)
      , color Color.mainPrimary
      , onClick push (const ResendOTP)
      , visibility if state.props.resendEnabled then VISIBLE else GONE
      ] <> FontStyle.tags TypoGraphy
    , textView $
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , text $  (getString RESEND_OTP_IN) <> "  " <> state.data.timer
      , visibility if state.props.resendEnabled then GONE else VISIBLE
      , color Color.blue900
      , alpha 0.6
      ] <> FontStyle.tags TypoGraphy
  ]
