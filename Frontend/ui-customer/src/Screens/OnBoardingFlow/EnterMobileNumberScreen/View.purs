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
import Components.StepsHeaderModel as StepsHeaderModel
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
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
import Prelude (Unit, bind, const, discard, not, pure, show, unit, when, ($), (&&), (/=), (<<<), (<>), (==), (>=))
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alpha, background, clickable, color, fontStyle, frameLayout, gravity, height, lineHeight, linearLayout, margin, onBackPressed, onClick, orientation, padding, singleLine, text, textSize, textView, visibility, weight, width, textFromHtml)
import PrestoDOM.Animation as PrestoAnim
import Screens.EnterMobileNumberScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color
import Merchant.Utils( getValueFromConfig )
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
          ] $ StepsHeaderModel.view (push <<< StepsHeaderModelAC) (StepsHeaderModel.config if state.props.enterOTP then 1 else 0)
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
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , visibility  if state.props.enterOTP then GONE else VISIBLE
    , alpha if state.props.enterOTP then 0.0 else 1.0
    , orientation VERTICAL
    ][PrestoAnim.animationSet
      [ Anim.translateYAnimFromTopWithAlpha translateYAnimConfig -- 300 10 0 0 true PrestoAnim.Linear
      ] $ PrimaryEditText.view (push <<< MobileNumberEditTextAction) (mobileNumberEditTextConfig state)
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , weight 1.0
      ][]
    , PrestoAnim.animationSet
      ( if EHC.os == "IOS" then [] else [ Anim.translateYAnimFromTopWithAlpha translateYAnimConfig -- 400 15 0 0 true PrestoAnim.Linear -- Temporary fix for iOS
      ]) $ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin (Margin 11 0 0 10)
        ][ commonTextView state (getString BY_TAPPING_CONTINUE) false Nothing push false
        , commonTextView state " &nbsp; <u>T&Cs</u>" true (Just (getValueFromConfig "DOCUMENT_LINK")) push true
          ]
    , PrestoAnim.animationSet
      [ Anim.fadeIn $ not state.props.enterOTP
      , Anim.fadeOut state.props.enterOTP
      ] $
      linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        ][PrimaryButton.view (push <<< MobileNumberButtonAction) (mobileNumberButtonConfig state)]
    ]

commonTextView :: ST.EnterMobileNumberScreenState -> String -> Boolean -> Maybe String -> (Action -> Effect Unit) -> Boolean -> forall w . PrestoDOM (Effect Unit) w
commonTextView state textValue isLink link push isTextFromHtml =
  textView
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , (if isTextFromHtml then textFromHtml else text) textValue
    , color if isLink then Color.blue900 else Color.black700
    , textSize FontSize.a_12
    , fontStyle $ FontStyle.medium LanguageStyle
    , onClick (\action -> do
                when isLink $ JB.openUrlInApp (fromMaybe "www.nammayatri.in" link)--"https://drive.google.com/file/d/1qYXbQUF4DVo2xNOawkHNTR_VVe46nggc/view?usp=sharing"
                pure unit
              ) (const TermsAndConditions)
    ]

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
      , clickable state.props.resendEnable
      ][linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , alpha if state.props.resendEnable then 1.0 else 0.5
      ][  textView
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text (getString RESEND)
        , textSize FontSize.a_12
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , color Color.blue900
        , onClick push (const Resend)
        ]
        , linearLayout
          [ width MATCH_PARENT
          , height (V 1)
          , background Color.blue900
          ][]
      ]
      , textView
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text if lang == "HI_IN" then ("  "<> show state.data.timer <> "s  "<> getString IN) else ("  " <> getString IN <> "  "<> show state.data.timer <> "  s")
        , textSize FontSize.a_12
        , lineHeight "22"
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , color Color.blue900
        , visibility if state.props.resendEnable then GONE else VISIBLE
        ]]
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

