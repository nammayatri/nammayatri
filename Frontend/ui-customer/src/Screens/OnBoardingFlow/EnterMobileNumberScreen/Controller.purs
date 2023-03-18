{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.EnterMobileNumberScreen.Controller where

import Components.GenericHeader.Controller as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.String.CodeUnits (charAt)
import Debug.Trace (spy)
import Engineering.Helpers.Commons (getNewIDWithTag, os, clearTimer)
import Helpers.Utils (setText')
import JBridge (hideKeyboardOnNavigation, toast, toggleBtnLoader,minimizeApp, firebaseLogEvent)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog, trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Prelude (class Show, bind, pure, unit, show, ($), (&&), (-), (<=), (==), (>), (||), discard)
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (EnterMobileNumberScreenState)
import Storage (KeyStore(..), setValueToLocalNativeStore)

instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = case action of
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
        VerifyOTPButtonAction act -> case act of
            PrimaryButtonController.OnClick -> do 
                trackAppActionClick appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "primary_button" "continue_otp"
                trackAppEndScreen appId (getScreen ENTER_MOBILE_NUMBER_SCREEN)
            PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "primary_button" "no_action"
        MobileNumberEditTextAction (PrimaryEditTextController.TextChanged id value) -> trackAppTextInput appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "mobilenumber_edit_text_changed" "primary_edit_text"
        OTPEditTextAction (PrimaryEditTextController.TextChanged id value) -> trackAppTextInput appId (getScreen ENTER_OTP_NUMBER_SCREEN) "otp_edit_text_changed" "primary_edit_text"
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

data ScreenOutput = GoToAccountSetUp EnterMobileNumberScreenState
                  | GoBack EnterMobileNumberScreenState
                  | GoToOTP EnterMobileNumberScreenState
                  | ResendOTP EnterMobileNumberScreenState
                  | GoToChooseLanguage EnterMobileNumberScreenState

data Action = EnterOTP 
            | BackPressed Boolean
            | AutoFill String
            | MobileNumberButtonAction PrimaryButtonController.Action 
            | VerifyOTPButtonAction PrimaryButtonController.Action 
            | MobileNumberEditTextAction PrimaryEditTextController.Action
            | OTPEditTextAction PrimaryEditTextController.Action
            | GenericHeaderActionController GenericHeaderController.Action
            | Resend
            | CountDown Int String String String 
            | NoAction
            | SetToken String
            | ContinueCommand
            | AfterRender
            | TermsAndConditions

eval :: Action -> EnterMobileNumberScreenState -> Eval Action ScreenOutput EnterMobileNumberScreenState

eval (MobileNumberButtonAction PrimaryButtonController.OnClick) state = continueWithCmd state [
        do
            _ <- pure $ firebaseLogEvent "ny_user_otp_triggered"
            _ <- pure $ hideKeyboardOnNavigation true
            let value = (if os == "IOS" then "" else " ")
            _ <- (setText' (getNewIDWithTag "EnterOTPNumberEditText") value )
            pure $ ContinueCommand 
]

eval (VerifyOTPButtonAction PrimaryButtonController.OnClick) state = do 
    _ <- pure $ hideKeyboardOnNavigation true
    updateAndExit state $ GoToAccountSetUp state

eval (MobileNumberEditTextAction (PrimaryEditTextController.TextChanged id value)) state = do 
    _ <- if length value == 10 then do
            pure $ hideKeyboardOnNavigation true 
            else pure unit
    let isValidMobileNumber = case (charAt 0 value) of 
                                    Just a -> if a=='0' || a=='1' || a=='2' || a=='3' || a=='4' then false 
                                                else if a=='5' then
                                                    if value=="5000500050" then true else false 
                                                        else true 
                                    Nothing -> true 
    if (length value == 10 && isValidMobileNumber) then do
        _ <- pure $ firebaseLogEvent "ny_user_mobnum_entry"
        pure unit
        else pure unit
    let newState = state { props = state.props { isValidMobileNumber = isValidMobileNumber
                                        , btnActiveMobileNuber = if (length value == 10 && isValidMobileNumber) then true else false}
                                        , data = state.data { mobileNumber = if length value <= 10 then value else state.data.mobileNumber}}  
    continue newState

eval (OTPEditTextAction (PrimaryEditTextController.TextChanged id value)) state = do 
    _ <- if length value == 4 then do
            pure $ hideKeyboardOnNavigation true 
            else pure unit
    let newState = state { props = state.props { btnActiveOTP = if length value == 4 then true else false, letterSpacing = if value == "" then 1.0 else 6.0, wrongOTP = if state.props.wrongOTP && value == "" then true else false}
                  , data = state.data { otp = if length value <= 4 then value else state.data.otp }}
    _ <- pure $ spy "entermobile number " state.props.letterSpacing
    continue newState

eval (GenericHeaderActionController (GenericHeaderController.PrefixImgOnClick )) state = continueWithCmd state [ do pure $ BackPressed state.props.enterOTP]

eval Resend state = do
    let newState = state {data{attempts = if (state.data.attempts > 0) then state.data.attempts - 1 else state.data.attempts},props{resendEnable = false}}
    if state.data.attempts == 0 then do
        _ <- pure $ toast (getString LIMIT_REACHED)
        continue newState{props{enterOTP = false}}
      else exit $ ResendOTP newState

eval (AutoFill otp) state = updateAndExit
    (state {props = state.props {
    isReadingOTP = if length otp == 4 then false else true ,
    capturedOtp = otp}, data = state.data { otp = if (length otp <= 4) then otp else state.data.otp}})
   (GoToAccountSetUp (state {props = state.props {isReadingOTP = if length otp == 4 then false else true ,capturedOtp = otp}, data = state.data { otp = if (length otp <= 4) then otp else state.data.otp}}))

eval (BackPressed flag) state = do 
      _ <- pure $ printLog "state" state
      _ <- pure $ toggleBtnLoader "" false
      let newState = state {props{enterOTP =  false,letterSpacing = 1.0},data{otp = ""}}
      _ <- pure $ hideKeyboardOnNavigation true
      if state.props.enterOTP then exit $ GoBack newState 
        else do
            _ <- pure $ minimizeApp ""
            continue state --exit $ GoToChooseLanguage newState{data{mobileNumber = ""}}-- Removed choose langauge screen

eval (CountDown seconds id status timerID) state = do 
        _ <- pure $ printLog "timer" seconds
        if status == "EXPIRED" then do 
            _ <- pure $ clearTimer timerID
            let newState = state{data{timer = "", timerID = ""},props = state.props{resendEnable = true}}
            continue newState
        else 
            continue $ state{data{timer = show seconds, timerID=timerID},props = state.props{resendEnable = false}}
eval (SetToken id )state = do 
  _ <- pure $ spy "SetTokenSetToken" id  
  _ <- pure $ setValueToLocalNativeStore FCM_TOKEN  id
  continue state

eval ContinueCommand state = exit $ GoToOTP state
eval AfterRender state = continue state

eval _ state = continue state