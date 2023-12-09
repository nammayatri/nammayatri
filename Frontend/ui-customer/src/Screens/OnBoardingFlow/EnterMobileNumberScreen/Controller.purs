{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.EnterMobileNumberScreen.Controller where

import Effect.Unsafe

import Common.Types.App (LazyCheck(..)) as Lazy
import Common.Types.App (MobileNumberValidatorResp(..)) as MVR
import Common.Types.App (OTPChannel(..)) as OTP
import Components.GenericHeader.Controller as GenericHeaderController
import Components.MobileNumberEditor.Controller as MobileNumberEditorController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.StepsHeaderModel.Controller as StepsHeaderModelController
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.String.CodeUnits (charAt)
import Debug (spy)
import Engineering.Helpers.Commons (getNewIDWithTag, os, clearTimer)
import Engineering.Helpers.LogEvent (logEvent)
import Engineering.Helpers.Utils (mobileNumberValidator, mobileNumberMaxLength)
import Helpers.Utils (setText, clearCountDownTimer, showCarouselScreen)
import JBridge (firebaseLogEvent, hideKeyboardOnNavigation, minimizeApp, toast, toggleBtnLoader)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog, trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Prelude (class Show, bind, pure, unit, show, ($), (&&), (-), (<=), (==), (>), (||), discard, void, when, not)
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit, LetterSpacing(..))
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (EnterMobileNumberScreenState)
import Storage (KeyStore(..), setValueToLocalNativeStore)

instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId =  case action of
        AfterRender -> trackAppScreenRender appId "screen" (getScreen ENTER_MOBILE_NUMBER_SCREEN)
        BackPressed flag -> do
            if flag then do
                trackAppBackPress appId (getScreen ENTER_OTP_NUMBER_SCREEN)
                trackAppEndScreen appId (getScreen ENTER_OTP_NUMBER_SCREEN)
            else do
                trackAppBackPress appId (getScreen ENTER_MOBILE_NUMBER_SCREEN)
                trackAppEndScreen appId (getScreen ENTER_MOBILE_NUMBER_SCREEN)
        MobileNumberButtonAction act -> case act of
            PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "primary_button" "continue_mobilenumber"
            PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "primary_button" "no_action"
        WhatsAppOTPButtonAction act -> case act of
            PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "primary_button" "continue_mobilenumber"
            PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "primary_button" "no_action"
        VerifyOTPButtonAction act -> case act of
            PrimaryButtonController.OnClick -> do
                trackAppActionClick appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "primary_button" "continue_otp"
                trackAppEndScreen appId (getScreen ENTER_MOBILE_NUMBER_SCREEN)
            PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "primary_button" "no_action"
        MobileNumberEditTextAction act -> case act of 
            MobileNumberEditorController.TextChanged id value -> trackAppTextInput appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "mobilenumber_edit_text_changed" "primary_edit_text"
            MobileNumberEditorController.FocusChanged _ -> trackAppTextInput appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "mobilenumber_edit_text_focus_changed" "primary_edit_text"
            MobileNumberEditorController.CountryCodeSelected _ -> trackAppTextInput appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "countrycode_edit_text_changed" "on_click_country_code"
            MobileNumberEditorController.ShowOptions -> trackAppTextInput appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "country_code_list_showed" "on_click_show"
            MobileNumberEditorController.CloseOptions -> trackAppTextInput appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "country_code_list_closed" "on_click_close"
        OTPEditTextAction act -> case act of 
            PrimaryEditTextController.TextChanged id value -> trackAppTextInput appId (getScreen ENTER_OTP_NUMBER_SCREEN) "otp_edit_text_changed" "primary_edit_text"
            PrimaryEditTextController.FocusChanged _ -> trackAppTextInput appId (getScreen ENTER_OTP_NUMBER_SCREEN) "otp_edit_text_focus_changed" "primary_edit_text"
        GenericHeaderActionController act -> case act of
            GenericHeaderController.PrefixImgOnClick -> do
                trackAppActionClick appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "generic_header_action" "back_icon_onclick"
                trackAppEndScreen appId (getScreen ENTER_MOBILE_NUMBER_SCREEN)
            GenericHeaderController.SuffixImgOnClick -> trackAppActionClick appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "generic_header_action" "forward_icon"
        Resend -> trackAppActionClick appId (getScreen ENTER_OTP_NUMBER_SCREEN) "in_screen" "resend_otp"
        CountDown seconds id status timerID -> trackAppScreenEvent appId (getScreen ENTER_OTP_NUMBER_SCREEN) "in_screen" "countdown_updated"
        AutoFill otp -> trackAppScreenEvent appId (getScreen ENTER_OTP_NUMBER_SCREEN) "in_screen" "autofill_otp"
        SetToken otp -> trackAppScreenEvent appId (getScreen ENTER_OTP_NUMBER_SCREEN) "in_screen" "settoken"
        ContinueCommand -> trackAppScreenEvent appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "in_screen" "continue_to_otp_screen"
        EnterOTP -> trackAppScreenEvent appId (getScreen ENTER_OTP_NUMBER_SCREEN) "in_screen" "enter_otp"
        TermsAndConditions -> trackAppActionClick appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "in_screen" "t_&_c"
        NoAction -> trackAppScreenEvent appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "in_screen" "no_action"
        StepsHeaderModelAC _ -> trackAppActionClick appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "steps_header_modal" "backpressed"
        SetPhoneNumber _ -> trackAppScreenEvent appId (getScreen ENTER_OTP_NUMBER_SCREEN) "in_screen" "set_phone_number"
        NonDisclosureAgreementAction -> trackAppActionClick appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "in_screen" "no_discloure_agreement"


data ScreenOutput = GoToAccountSetUp EnterMobileNumberScreenState
                  | GoBack EnterMobileNumberScreenState
                  | GoToOTP EnterMobileNumberScreenState
                  | ResendOTP EnterMobileNumberScreenState
                  | GoToWelcomeScreen EnterMobileNumberScreenState


data Action = EnterOTP
            | BackPressed Boolean
            | AutoFill String
            | MobileNumberButtonAction PrimaryButtonController.Action
            | VerifyOTPButtonAction PrimaryButtonController.Action
            | MobileNumberEditTextAction MobileNumberEditorController.Action
            | OTPEditTextAction PrimaryEditTextController.Action
            | GenericHeaderActionController GenericHeaderController.Action
            | Resend
            | CountDown Int String String String
            | NoAction
            | SetToken String
            | ContinueCommand
            | AfterRender
            | TermsAndConditions
            | StepsHeaderModelAC StepsHeaderModelController.Action
            | SetPhoneNumber String
            | NonDisclosureAgreementAction
            | WhatsAppOTPButtonAction PrimaryButtonController.Action

eval :: Action -> EnterMobileNumberScreenState -> Eval Action ScreenOutput EnterMobileNumberScreenState

eval (MobileNumberButtonAction PrimaryButtonController.OnClick) state = do
    let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_otp_triggered"
    let newState = state {data {otpChannel = OTP.SMS}, props{btnActiveOTP = false}}
    pure $ hideKeyboardOnNavigation true
    pure $ setText (getNewIDWithTag "EnterOTPNumberEditText") ""
    exit $ GoToOTP newState

eval (WhatsAppOTPButtonAction PrimaryButtonController.OnClick) state = do
    let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_otp_triggered"
    let newState = state {data {otpChannel = OTP.WHATSAPP}}
    pure $ hideKeyboardOnNavigation true
    pure $ setText (getNewIDWithTag "EnterOTPNumberEditText") ""
    exit $ GoToOTP newState

eval (StepsHeaderModelAC StepsHeaderModelController.OnArrowClick) state = continueWithCmd state [ do pure $ BackPressed state.props.enterOTP]

eval (VerifyOTPButtonAction PrimaryButtonController.OnClick) state = do
    _ <- pure $ hideKeyboardOnNavigation true
    _ <- pure $ clearCountDownTimer state.data.timerID
    updateAndExit state $ GoToAccountSetUp state

eval (MobileNumberEditTextAction (MobileNumberEditorController.TextChanged id value)) state = do
    _ <- if length value ==  mobileNumberMaxLength state.data.countryObj.countryShortCode then do
            pure $ hideKeyboardOnNavigation true
            else pure unit
    let validatorResp = mobileNumberValidator state.data.countryObj.countryCode state.data.countryObj.countryShortCode value 
    let newState = state { props = state.props { isValidMobileNumber = true --isValidPrefixMobileNumber validatorResp
                                        , btnActiveMobileNumber = isValidMobileNumber validatorResp
                                        , countryCodeOptionExpanded = false}
                                        , data = state.data { mobileNumber = if validatorResp == MVR.MaxLengthExceeded then state.data.mobileNumber else value}}
    if  isValidMobileNumber validatorResp then do 
         let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_mobnum_entry"
         pure unit
     else pure unit

    continue newState

eval (MobileNumberEditTextAction (MobileNumberEditorController.FocusChanged mobileNumberFocused)) state = continue state { props{ mNumberEdtFocused = mobileNumberFocused, countryCodeOptionExpanded = not mobileNumberFocused}}

eval (MobileNumberEditTextAction (MobileNumberEditorController.CountryCodeSelected country)) state = do 
    _ <-  pure $ hideKeyboardOnNavigation true
    let validatorResp = mobileNumberValidator country.countryCode country.countryShortCode state.data.mobileNumber 
    let newState = state {data {countryObj = country} 
                        , props {countryCodeOptionExpanded = false, mNumberEdtFocused = true
                        , isValidMobileNumber = isValidPrefixMobileNumber validatorResp
                        , btnActiveMobileNumber = isValidMobileNumber validatorResp}}
    if isValidMobileNumber validatorResp then do 
         let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_mobnum_entry"
         pure unit
    else pure unit
    continue newState

eval (MobileNumberEditTextAction MobileNumberEditorController.ShowOptions) state = continue state{props {countryCodeOptionExpanded = true, mNumberEdtFocused = false}}

eval (MobileNumberEditTextAction MobileNumberEditorController.CloseOptions) state = continue state {props {countryCodeOptionExpanded = false}}

eval (OTPEditTextAction (PrimaryEditTextController.FocusChanged boolean)) state = continue state { props{ otpEdtFocused = boolean}}

eval (OTPEditTextAction (PrimaryEditTextController.TextChanged id value)) state = do
    let newState = state { props = state.props { btnActiveOTP = if length value == 4 then true else false, letterSpacing = PX if value == "" then 1.0 else 6.0, wrongOTP = if state.props.wrongOTP && value == "" then true else false}
                  , data = state.data { otp = if length value <= 4 then value else state.data.otp }}
    if length value == 4 then do
        pure $ hideKeyboardOnNavigation true
        _ <- pure $ clearCountDownTimer state.data.timerID
        updateAndExit newState $ GoToAccountSetUp newState
    else
        continue newState

eval (GenericHeaderActionController (GenericHeaderController.PrefixImgOnClick )) state = continueWithCmd state [ do pure $ BackPressed state.props.enterOTP]

eval Resend state = do
    let newState = state {data{attempts = if (state.data.attempts > 0) then state.data.attempts - 1 else state.data.attempts},props{resendEnable = false}}
    if state.data.attempts == 0 then do
        _ <- pure $ toast (getString OTP_ENTERING_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER)
        _ <- pure $ toggleBtnLoader "" false
        continue newState{props{enterOTP = false}}
      else do 
        _ <- pure $ toast (getString OTP_RESENT_SUCCESSFULLY)
        exit $ ResendOTP newState

eval (AutoFill otp) state = do
    _ <- pure $ firebaseLogEvent "ny_user_otp_autoread"
    let newState = state {props = state.props {isReadingOTP = if length otp == 4 then false else true ,capturedOtp = otp}, data = state.data { otp = if (length otp <= 4) then otp else state.data.otp}}
    updateAndExit newState $ GoToAccountSetUp (newState)
   
eval (BackPressed flag) state = do
      _ <- pure $ spy "state" state
      _ <- pure $ toggleBtnLoader "" false
      _ <- pure $ clearCountDownTimer state.data.timerID
      let newState = state {props{enterOTP =  false,letterSpacing = PX 1.0},data{otp = ""}}
      _ <- pure $ hideKeyboardOnNavigation true
      if state.props.enterOTP then exit $ GoBack newState
        else if showCarouselScreen Lazy.FunctionCall then exit $ GoToWelcomeScreen newState{data{mobileNumber = ""}} 
            else do 
                void $ pure $ minimizeApp ""
                continue state

eval (CountDown seconds id status timerID) state = do
        _ <- pure $ printLog "timer" $ show seconds
        if status == "EXPIRED" then do
            _ <- pure $ clearCountDownTimer state.data.timerID
            let newState = state{data{timer = 30, timerID = ""},props = state.props{resendEnable = true}}
            continue newState
        else
            continue $ state{data{timer = seconds, timerID=timerID},props = state.props{resendEnable = false}}
eval (SetToken id )state = do
  _ <- pure $ spy "SetTokenSetToken" id
  _ <- pure $ setValueToLocalNativeStore FCM_TOKEN  id
  continue state

eval (SetPhoneNumber number )state = continue state {props { editTextVal = number, mNumberEdtFocused = true}}

eval ContinueCommand state = exit $ GoToOTP state{data{timer = 30, timerID = ""},props = state.props{btnActiveOTP = false, resendEnable = false}}
eval AfterRender state = continue state

eval _ state = continue state

isValidPrefixMobileNumber :: MVR.MobileNumberValidatorResp -> Boolean 
isValidPrefixMobileNumber resp = (resp == MVR.ValidPrefix || resp == MVR.Valid)

isValidMobileNumber :: MVR.MobileNumberValidatorResp -> Boolean 
isValidMobileNumber resp = (resp == MVR.Valid)