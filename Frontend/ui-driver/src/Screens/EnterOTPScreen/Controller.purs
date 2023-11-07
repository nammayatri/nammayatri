{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.EnterOTPScreen.Controller where

import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText.Controllers as PrimaryEditText
import Components.StepsHeaderModal.Controller as StepsHeaderModalController
import Data.String (length)
import JBridge (hideKeyboardOnNavigation, firebaseLogEvent)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Prelude (class Show, not, pure, unit, (&&), (<=), (==), (||), ($), discard, bind, (>=))
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (EnterOTPScreenState)
import Storage (setValueToLocalNativeStore, KeyStore(..))
import Storage (setValueToLocalNativeStore, setValueToLocalStore, KeyStore(..))

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen ENTER_OTP_NUMBER_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen ENTER_OTP_NUMBER_SCREEN)
      trackAppEndScreen appId (getScreen ENTER_OTP_NUMBER_SCREEN)
    ResendOTP -> trackAppActionClick appId (getScreen ENTER_OTP_NUMBER_SCREEN) "in_screen" "resend_otp" 
    PrimaryEditTextAction act -> case act of
      PrimaryEditText.OnClick -> trackAppActionClick appId (getScreen ENTER_OTP_NUMBER_SCREEN) "primary_edit_text" "on_click"
      PrimaryEditText.TextChanged valId newVal -> trackAppTextInput appId (getScreen ENTER_OTP_NUMBER_SCREEN) "otp_number_text_changed" "primary_edit_text"
      PrimaryEditText.TextClicked -> trackAppActionClick appId (getScreen ENTER_OTP_NUMBER_SCREEN) "primary_edit_text" "text_field_click"
    PrimaryButtonActionController act -> case act of
      PrimaryButton.OnClick-> do
        trackAppActionClick appId (getScreen ENTER_OTP_NUMBER_SCREEN) "in_screen" "primary_button_register_on_click"
        trackAppEndScreen appId (getScreen ENTER_OTP_NUMBER_SCREEN)
      PrimaryButton.NoAction -> trackAppActionClick appId (getScreen ENTER_OTP_NUMBER_SCREEN) "in_screen" "primary_button_no_action"
    TIMERACTION time-> trackAppScreenEvent appId (getScreen ENTER_OTP_NUMBER_SCREEN) "in_screen" "timer_action"
    AutoFill otp-> trackAppScreenEvent appId (getScreen ENTER_OTP_NUMBER_SCREEN) "in_screen" "otp_autofill"
    SetToken id -> trackAppScreenEvent appId (getScreen ENTER_OTP_NUMBER_SCREEN) "in_screen" "set_token"
    NoAction -> trackAppScreenEvent appId (getScreen ENTER_OTP_NUMBER_SCREEN) "in_screen" "no_action"
    _ -> trackAppScreenEvent appId (getScreen ENTER_OTP_NUMBER_SCREEN) "in_screen" "no_action"
    
data ScreenOutput = GoBack  EnterOTPScreenState | GoToHome EnterOTPScreenState | Retry EnterOTPScreenState
data Action = BackPressed 
            | ResendOTP
            | PrimaryEditTextAction PrimaryEditText.Action
            | PrimaryButtonActionController PrimaryButton.Action
            | NoAction
            | AutoFill String 
            | SetToken String
            | TIMERACTION String
            | AfterRender
            | StepsHeaderModelAC StepsHeaderModalController.Action

eval :: Action -> EnterOTPScreenState -> Eval Action ScreenOutput EnterOTPScreenState
eval AfterRender state = continue state
eval BackPressed state = do 
  exit $ GoBack state{props{isValid = false}} 
eval (PrimaryEditTextAction PrimaryEditText.OnClick) state = continue state
eval (PrimaryButtonActionController (PrimaryButton.OnClick)) state = do
  _ <- pure $ hideKeyboardOnNavigation true 
  exit (GoToHome state)
eval (ResendOTP) state = do
  if state.props.resendEnabled then exit (Retry state)
  else continue state
eval (TIMERACTION time) state = do
    if time == "EXPIRED" then do 
      continue state { data = state.data {timer = "10s"}, props = state.props{resendEnabled = true}}
      else continue $ state {  data = state.data {timer = time }, props = state.props{resendEnabled = false}}
eval (PrimaryEditTextAction (PrimaryEditText.TextChanged valId newVal)) state = do
  let newState = state { props = state.props { btnActive = if length newVal == 4 then true else false, isValid = false}
                  , data = state.data { otp = if length newVal <= 4 then newVal else state.data.otp }}
  if length newVal >= 4 then do
      _ <- pure $ hideKeyboardOnNavigation true
      exit (GoToHome newState)
      else continue newState
  

eval (AutoFill otpReceived) state = do 
  _ <- pure $ firebaseLogEvent "ny_driver_otp_autoread"
  updateAndExit (state { data {capturedOtp = otpReceived, otp = if (length otpReceived) == 4 then otpReceived else state.data.otp } }) $ GoToHome (state { data {capturedOtp = otpReceived, otp = if (length otpReceived) == 4 then otpReceived else state.data.otp } })
eval (SetToken id )state = do 
  _ <-  pure $ setValueToLocalNativeStore FCM_TOKEN  id
  _ <-  pure $ setValueToLocalStore FCM_TOKEN  id
  continue state

eval (StepsHeaderModelAC StepsHeaderModalController.OnArrowClick) state = continueWithCmd state [ do pure $ BackPressed]

eval _ state = continue state