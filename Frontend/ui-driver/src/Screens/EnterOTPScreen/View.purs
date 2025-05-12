{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.EnterOTPScreen.View where
import Data.Maybe (Maybe(..))
import Prelude (Unit, const, bind, pure, unit, ($), (<<<), (<>), (==), (>), discard)
import PrestoDOM (Gravity(..), Length(..), LetterSpacing(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), alpha, background, clickable, color, fontStyle, gravity, height, imageUrl, imageView, linearLayout, margin, onBackPressed, onClick, orientation, padding, text, textSize, textView, weight, width, afterRender, visibility, imageWithFallback, textFromHtml)
import Components.PrimaryEditText.Views as PrimaryEditText
import Components.PrimaryButton as PrimaryButton
import Effect (Effect)
import Screens.EnterOTPScreen.Controller (Action(..), eval, ScreenOutput)
import Screens.Types as ST
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types(STR(..))
import Engineering.Helpers.Commons as EHC
import JBridge as JB
import Helpers.Utils as HU
import Data.String as DS
import PrestoDOM.Animation as PrestoAnim
import Animation as Anim
import Animation.Config as AnimConfig
import Common.Types.App
import Screens.EnterOTPScreen.ComponentConfig
import Data.Ring ((-))
import Storage (getValueToLocalStore, KeyStore(..))
import Locale.Utils

screen :: ST.EnterOTPScreenState -> Screen Action ST.EnterOTPScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "EnterOTPScreen"
  , globalEvents : [ (\push -> do
                      _ <- pure $ HU.clearTimer ""
                      _ <- HU.startTimer 10 true push TIMERACTION
                      pure (pure unit)) ] <> if (DS.length initialState.data.otp) > 0 then [] else [] --[ HU.startOtpReciever AutoFill ]
  , eval
  }

view
  :: forall w
  . (Action -> Effect Unit)
  -> ST.EnterOTPScreenState
  -> PrestoDOM (Effect Unit) w
view push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.white900
  , margin $ MarginBottom 24
  , afterRender (\action -> do
        _ <- push action
        _ <- JB.setFCMToken push $ SetToken
        _ <- JB.requestKeyboardShow (EHC.getNewIDWithTag "EnterOTPScreenEditText")
        pure unit
        ) (const AfterRender)
  , onBackPressed push (const BackPressed)
  ][    PrestoAnim.animationSet
          [ Anim.fadeIn true
          ] $ headerView state push 
      , linearLayout
      [ width MATCH_PARENT
      , weight 1.0
      , orientation VERTICAL
      ][  primaryEditTextView state push
      ]
    , PrestoAnim.animationSet
      [ Anim.fadeIn true
      ] $ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          ][PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonViewConfig state)]
  ]

  where 
    headerView :: ST.EnterOTPScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
    headerView state push = 
      linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background state.data.config.primaryBackground
      , padding $ Padding 16 16 16 16
      ][  imageView
          [ imageWithFallback $ HU.fetchImage HU.FF_ASSET state.data.config.themeColors.defaultBackButton
          , height $ V 25 
          , width $ V 25
          , onClick push $ const BackPressed
          ]
        , textView $ 
          [ text $ getString GOT_AN_OTP
          , color state.data.config.themeColors.onboardingHeaderTextColor
          , margin $ MarginVertical 5 22
          , height WRAP_CONTENT
          , width MATCH_PARENT
          ] <> FontStyle.h1 TypoGraphy
      ]




--------------------- backArrow ----------------------------
backArrow :: ST.EnterOTPScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
backArrow state push =
 linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , padding (Padding 16 16 16 0)
  , margin (MarginTop 10)
  ][  imageView
      [ width (V 25)
      , height (V 25)
      , margin (MarginTop 20)
      , imageWithFallback $ HU.fetchImage HU.FF_ASSET "ny_ic_back"
      , onClick push (const BackPressed)
      ]
  ]


------------------------- enterOTPTextView -------------------
enterOTPTextView :: ST.EnterOTPScreenState -> forall w . PrestoDOM (Effect Unit) w
enterOTPTextView state=
 textView
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , textSize FontSize.a_26
  , text (getString ENTER_OTP)
  , fontStyle $ FontStyle.bold LanguageStyle
  , color Color.textPrimary
  , margin (Margin 16 28 0 0)
  ]


---------------------------- primaryEditTextView -------------------

primaryEditTextView :: ST.EnterOTPScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
primaryEditTextView state push =
 linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , padding (Padding 18 18 20 0)
  , orientation VERTICAL
  ][  PrestoAnim.animationSet
        [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
        ] $ PrimaryEditText.view(push <<< PrimaryEditTextAction) ({
        title: case (getLanguageLocale languageKey) of 
                  "EN_US" -> (getString OTP_SENT_TO) <> state.data.mobileNo
                  _ -> state.data.mobileNo <> (getString ENTER_OTP_SENT_TO) ,
        type: "number",
        hint: (getString AUTO_READING_OTP),
        text: state.data.capturedOtp,
        isinValid: state.props.isValid,
        error: Just (getString PLEASE_ENTER_VALID_OTP),
        valueId: "EditTextOtp",
        pattern : Just "[0-9]*,4",
        fontSize : FontSize.a_18,
        letterSpacing : PX if state.data.otp == "" then 0.0 else 5.0,
        id : (EHC.getNewIDWithTag "EnterOTPScreenEditText"),
        background : state.data.config.themeColors.radioInactiveBackground,
        stroke : state.data.config.themeColors.editTextNormalStroke
      })
    , PrestoAnim.animationSet
      [ Anim.translateYAnimFromTopWithAlpha AnimConfig.translateYAnimConfig
      ] $ underlinedTextView state push
  ]

--------------------------------- underlinedTextView ----------------------
underlinedTextView :: ST.EnterOTPScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
underlinedTextView state push =
 linearLayout
 [ width WRAP_CONTENT
 , height WRAP_CONTENT
 , margin (MarginTop 18)
 ][ textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text $ getString DIDNT_RECIEVE_OTP
        , orientation HORIZONTAL
        , visibility if state.props.resendEnabled then VISIBLE else GONE
        ]<> FontStyle.body6 LanguageStyle
  ,linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , onClick push (const ResendOTP)
      ][  textView $
          [ height WRAP_CONTENT
          , weight 1.0
          , textFromHtml (getString RESEND_OTP)
          , color Color.mainPrimary
          , visibility if state.props.resendEnabled then VISIBLE else GONE
          ] <> FontStyle.body6 LanguageStyle
        , textView $
          [ height WRAP_CONTENT
          , weight 1.0
          , textFromHtml $  (getString RESEND_OTP_IN) <> "  " <> state.data.timer
          , visibility if state.props.resendEnabled then GONE else VISIBLE
          , color if state.props.resendEnabled then Color.mainPrimary else Color.black700
          ] <> FontStyle.body6 LanguageStyle
      ]
 ]
