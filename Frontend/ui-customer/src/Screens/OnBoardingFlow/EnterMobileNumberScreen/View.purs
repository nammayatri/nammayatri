{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.EnterMobileNumberScreen.View where

import Common.Types.App
import Screens.OnBoardingFlow.EnterMobileNumberScreen.ComponentConfig

import Animation as Anim
import Animation.Config (translateYAnimConfig)
import Components.GenericHeader as GenericHeader
import Components.MobileNumberEditor as MobileNumberEditor
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Components.StepsHeaderModel as StepsHeaderModel
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (startOtpReciever)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog)
import MerchantConfig.Utils (getValueFromConfig)
import Prelude (Unit, bind, const, discard, not, pure, show, unit, when, ($), (&&), (/=), (<<<), (<>), (==), (>=), (||), (-))
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), Accessiblity(..), afterRender, alpha, background, clickable, color, fontStyle, frameLayout, gravity, height, lineHeight, linearLayout, margin, onBackPressed, onClick, orientation, padding, singleLine, text, textSize, textView, visibility, weight, width, textFromHtml, accessibility, accessibilityHint)
import PrestoDOM.Animation as PrestoAnim
import Screens.EnterMobileNumberScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color
import Types.App (defaultGlobalState)

screen :: ST.EnterMobileNumberScreenState -> Screen Action ST.EnterMobileNumberScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "EnterMobileNumberScreen"
  , globalEvents : [ (\push ->
    do
      _ <- JB.setFCMToken push $ SetToken
      if not initialState.props.enterOTP then JB.detectPhoneNumbers push $ SetPhoneNumber else pure unit
      if initialState.data.timerID == "" then pure unit else pure $ EHC.clearTimer initialState.data.timerID
      if not initialState.props.resendEnable && initialState.data.attempts >= 0 && initialState.props.enterOTP then do
          _ <- launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ lift $ lift $ doAff do 
            if (EHC.os == "IOS") then liftEffect $ JB.startTimerWithTime (show initialState.data.timer) "otp" "1" push CountDown
            else  liftEffect $ EHC.countDown initialState.data.timer "otp" push CountDown
          pure unit
        else pure unit
      pure (pure unit)) ] <> if EHC.os == "IOS" then [] else [ startOtpReciever AutoFill ]
  , eval : \action state -> do
      let _ = printLog  "EnterMobileNumber state -----" state
      eval action state
  }

view
  :: forall w
  . (Action -> Effect Unit) -> ST.EnterMobileNumberScreenState -> PrestoDOM (Effect Unit) w
view push state = let
  lang = getValueToLocalStore LANGUAGE_KEY
   in
   linearLayout
   [  height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
   ][  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , afterRender (\action -> do
        _ <- push action
        -- _ <- requestKeyboardShow (getNewIDWithTag "EnterMobileNumberEditText")
        pure unit
        ) (const AfterRender)
    , margin $ MarginBottom 24
    , padding (Padding 0 EHC.safeMarginTop 0 EHC.safeMarginBottom)
    , background Color.white900
    , onBackPressed push (const BackPressed state.props.enterOTP)
    ][  PrestoAnim.animationSet
          [ Anim.fadeIn true
          ] $ StepsHeaderModel.view (push <<< StepsHeaderModelAC) $ (StepsHeaderModel.stepsHeaderData if state.props.enterOTP then 1 else 0){config = state.data.config}
      , frameLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , padding (Padding 16 0 16 0)
        ][
          PrestoAnim.animationSet
            [ Anim.fadeOut state.props.enterOTP
            , Anim.fadeIn  (not state.props.enterOTP)
            ] $
            enterMobileNumberView  state lang push
          -- , PrestoAnim.animationSet
          --   [ Anim.fadeIn state.props.enterOTP
          --   , Anim.fadeOut  (not state.props.enterOTP)
          --   ]  $
          , if state.props.enterOTP then (enterOTPView state lang push) else textView[]
          ]
      ]
    ]

---------------------------------- enterMobileNumberView -----------------------------------
enterMobileNumberView:: ST.EnterMobileNumberScreenState  -> String -> (Action -> Effect Unit)  -> forall w . PrestoDOM (Effect Unit) w
enterMobileNumberView  state lang push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , visibility  if state.props.enterOTP then GONE else VISIBLE
    , alpha if state.props.enterOTP then 0.0 else 1.0
    , orientation VERTICAL
    ][
        MobileNumberEditor.view (push <<< MobileNumberEditTextAction) (mobileNumberEditTextConfig state)
       ,  PrestoAnim.animationSet
          ( if EHC.os == "IOS" then [] else [ Anim.translateYAnimFromTopWithAlpha translateYAnimConfig 
          , Anim.fadeOut state.props.countryCodeOptionExpanded
          ]) $ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , margin (Margin 0 8 0 12)
            , visibility  if state.props.countryCodeOptionExpanded then GONE else VISIBLE
            ][ commonTextView state (getString BY_TAPPING_CONTINUE) false Nothing push false false ""
            , commonTextView state " &nbsp; <u>T&Cs</u>" true (Just (getValueFromConfig "DOCUMENT_LINK")) push true true ( " By Clicking Continue: You Agree To Our Terms And Conditions" )
              ]
        , PrestoAnim.animationSet
          [ Anim.fadeIn $ not state.props.enterOTP 
          , Anim.fadeOut $ state.props.enterOTP || state.props.countryCodeOptionExpanded
          ] $
          linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , visibility  if state.props.countryCodeOptionExpanded then GONE else VISIBLE
            ][PrimaryButton.view (push <<< MobileNumberButtonAction) (mobileNumberButtonConfig state)]
        , if (not state.props.countryCodeOptionExpanded) && (elem state.data.countryObj.countryShortCode state.data.config.enableWhatsappOTP) then whatsAppOTPButtonView state push else dummyView push
    ]

whatsAppOTPButtonView  :: ST.EnterMobileNumberScreenState  -> (Action -> Effect Unit)  -> forall w . PrestoDOM (Effect Unit) w
whatsAppOTPButtonView state push = 
  linearLayout [
    height WRAP_CONTENT,
    width MATCH_PARENT, 
    orientation VERTICAL,
    gravity CENTER
  ][PrestoAnim.animationSet [Anim.fadeOut state.props.countryCodeOptionExpanded] $ 
    textView $ [
      height WRAP_CONTENT
    , width MATCH_PARENT
    , text $ getString OR
    , gravity CENTER
    , margin $ MarginVertical 24 24
    , color Color.black500
    ] <> FontStyle.tags TypoGraphy
  , PrestoAnim.animationSet
    [ Anim.fadeIn $ not state.props.enterOTP
    , Anim.fadeOut $ state.props.countryCodeOptionExpanded || state.props.enterOTP
    ] $
    linearLayout
      [ height WRAP_CONTENT
      , width $ V (EHC.screenWidth unit - 32)
      , gravity CENTER
      ][PrimaryButton.view (push <<< WhatsAppOTPButtonAction) (whatsAppOTPButtonConfig state)]
    ]


commonTextView :: ST.EnterMobileNumberScreenState -> String -> Boolean -> Maybe String -> (Action -> Effect Unit) -> Boolean -> Boolean -> String-> forall w . PrestoDOM (Effect Unit) w
commonTextView state textValue isLink link push isTextFromHtml enableAccessibilityHint accessibilityText=
  textView $
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , (if isTextFromHtml then textFromHtml else text) textValue
    , color if isLink then Color.blue900 else Color.black700
    , accessibility $ if enableAccessibilityHint then ENABLE else DISABLE
    , accessibilityHint accessibilityText
    , onClick (\action -> do
                when isLink $ JB.openUrlInApp (fromMaybe "www.nammayatri.in" link)--"https://drive.google.com/file/d/1qYXbQUF4DVo2xNOawkHNTR_VVe46nggc/view?usp=sharing"
                pure unit
              ) (const TermsAndConditions)
    ] <> FontStyle.tags TypoGraphy

------------------------------------- enterOTPView --------------------------------------------
enterOTPView:: ST.EnterMobileNumberScreenState -> String -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
enterOTPView state lang push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , alpha if state.props.enterOTP then 1.0 else 0.0
    , orientation VERTICAL
    ][PrestoAnim.animationSet
      [ Anim.translateYAnimFromTopWithAlpha translateYAnimConfig{ifAnim = state.props.enterOTP} --400 15 0 0 state.props.enterOTP PrestoAnim.Linear
      ] $ PrimaryEditText.view (push <<< OTPEditTextAction) (otpEditTextConfig state)
    ,  linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , orientation HORIZONTAL
      , alpha if state.props.resendEnable then 1.0 else 0.5
      ][linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      
      ][  textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text (getString RESEND)
          , clickable state.props.resendEnable
          , color Color.blue900
          , accessibility ENABLE
          , accessibilityHint if state.props.resendEnable then "Resend : Button" else ("Resend in " <> show state.data.timer <> "seconds")
          , onClick push (const Resend)
          ] <> FontStyle.body9 TypoGraphy
        , linearLayout
          [ width MATCH_PARENT
          , height (V 1)
          , background Color.blue900
          ][]
      ]
      , textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text if lang == "HI_IN" then ("  "<> show state.data.timer <> "s  "<> getString IN) else ("  " <> getString IN <> "  "<> show state.data.timer <> "  s")
        , color Color.blue900
        , accessibility DISABLE
        , visibility if state.props.resendEnable then GONE else VISIBLE
        ] <> FontStyle.body9 TypoGraphy]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , weight 1.0
      ][]
    , PrestoAnim.animationSet
      [ Anim.fadeIn state.props.enterOTP
      ] $
      linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      ][PrimaryButton.view (push <<< VerifyOTPButtonAction) (verifyOTPButtonConfig state)]
    ]

dummyView :: forall w . (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
dummyView push  = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  ][]
