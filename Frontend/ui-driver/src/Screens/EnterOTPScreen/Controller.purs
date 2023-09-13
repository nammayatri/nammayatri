{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.EnterOTPScreen.Controller where
import Prelude (class Show, not, pure, unit, (&&), (<=), (==), (||), ($), discard, bind, (>=))
import PrestoDOM (Eval, continue, exit, updateAndExit,continueWithCmd,onFocus,onChange,toast)
import Screens.Types (EnterOTPScreenState)
import Components.OtpPrimaryEditText.Controller as OtpPrimaryEditText
import Components.PrimaryButton as PrimaryButton
import Components.StepsHeaderModel.Controller as StepsHeaderModelController
import PrestoDOM.Types.Core (class Loggable)
import Data.String (length)
import JBridge (hideKeyboardOnNavigation)
import Storage (setValueToLocalNativeStore, setValueToLocalStore, KeyStore(..))
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Storage (setValueToLocalNativeStore, KeyStore(..))
import Screens (ScreenName(..), getScreen)
import PrestoDOM.Animation as PrestoAnim
import Animation.Config as AnimConfig
import Common.Types.App
import Effect (Effect)
import Effect (Effect)
import Prelude (Unit, ($), const, unit, not,(<>),(/),(-), (==))
import Font.Size as FontSize
import Font.Style as FontStyle
import Styles.Colors as Color
import PrestoDOM (PrestoDOM, Orientation(..), Gravity(..), Length(..), Padding(..), Margin(..), Visibility(..), margin, padding, orientation, height, width, linearLayout, imageView, imageUrl, text, textView, textSize, fontStyle, gravity, clickable, onClick, color, background, lineHeight, visibility, cornerRadius, stroke, ellipsize, maxLines, imageWithFallback, weight)
import Language.Strings (getString)
import Language.Types (STR(..))
import Common.Types.App
import Engineering.Helpers.Commons as EHC
import Data.Maybe(Maybe(..), fromMaybe)
import Debug (spy)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen ENTER_OTP_NUMBER_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen ENTER_OTP_NUMBER_SCREEN)
      trackAppEndScreen appId (getScreen ENTER_OTP_NUMBER_SCREEN)
    ResendOTP -> trackAppActionClick appId (getScreen ENTER_OTP_NUMBER_SCREEN) "in_screen" "resend_otp" 
    OtpPrimaryEditTextAction act -> case act of
      OtpPrimaryEditText.OnClickTextBox id -> trackAppActionClick appId (getScreen ENTER_OTP_NUMBER_SCREEN) "otp_primary_edit_text" "on_click"
      OtpPrimaryEditText.FocusChanged id -> trackAppActionClick appId (getScreen ENTER_OTP_NUMBER_SCREEN) "primary_edit_text" "on_click"
      OtpPrimaryEditText.TextChanged valId newVal -> trackAppTextInput appId (getScreen ENTER_OTP_NUMBER_SCREEN) "otp_number_text_changed" "primary_edit_text"
      --PrimaryEditText.TextClicked -> trackAppActionClick appId (getScreen ENTER_OTP_NUMBER_SCREEN) "primary_edit_text" "text_field_click"
    PrimaryButtonActionController act -> case act of
      PrimaryButton.OnClick-> do
        trackAppActionClick appId (getScreen ENTER_OTP_NUMBER_SCREEN) "in_screen" "primary_button_register_on_click"
        trackAppEndScreen appId (getScreen ENTER_OTP_NUMBER_SCREEN)
      PrimaryButton.NoAction -> trackAppActionClick appId (getScreen ENTER_OTP_NUMBER_SCREEN) "in_screen" "primary_button_no_action"
    TIMERACTION time-> trackAppScreenEvent appId (getScreen ENTER_OTP_NUMBER_SCREEN) "in_screen" "timer_action"
    AutoFill otp-> trackAppScreenEvent appId (getScreen ENTER_OTP_NUMBER_SCREEN) "in_screen" "otp_autofill"
    SetToken id -> trackAppScreenEvent appId (getScreen ENTER_OTP_NUMBER_SCREEN) "in_screen" "set_token"
    NoAction -> trackAppScreenEvent appId (getScreen ENTER_OTP_NUMBER_SCREEN) "in_screen" "no_action"
    _ ->  trackAppScreenEvent appId (getScreen ENTER_OTP_NUMBER_SCREEN) "in_screen" "Extra"
    
data ScreenOutput = GoBack  EnterOTPScreenState | GoToHome EnterOTPScreenState | Retry EnterOTPScreenState
data Action = BackPressed 
            | ResendOTP
            | OtpPrimaryEditTextAction OtpPrimaryEditText.Action
            | PrimaryButtonActionController PrimaryButton.Action
            | NoAction
            | AutoFill String 
            | SetToken String
            | TIMERACTION String
            | AfterRender
            | StepsHeaderModelAC StepsHeaderModelController.Action
            | TermsAndConditions
            | TextChng String 

eval :: Action -> EnterOTPScreenState -> Eval Action ScreenOutput EnterOTPScreenState
eval AfterRender state = continue state
eval BackPressed state = do 
  exit $ GoBack state{props{isValid = false}} 
eval (OtpPrimaryEditTextAction (OtpPrimaryEditText.FocusChanged id)) state = continue state
eval (OtpPrimaryEditTextAction (OtpPrimaryEditText.OnClickTextBox id)) state = do
        continue state {data {editTextId = id},props { otpTmp = true}}
eval (PrimaryButtonActionController (PrimaryButton.OnClick)) state  = do
  _ <- pure $ hideKeyboardOnNavigation true 
  exit (GoToHome state)
eval (ResendOTP) state = do
        _ <- pure $ toast ("hello")
        _ <- pure $ spy  "hello" state.data.editTextId
        continue state {props { otpTmp = true}}
  -- if state.props.resendEnabled then exit (Retry state)
  -- else continue state
eval (TIMERACTION time) state = do
    if time == "EXPIRED" then do 
      continue state { data = state.data {timer = "10s"}, props = state.props{resendEnabled = true}}
      else continue $ state {  data = state.data {timer = time }, props = state.props{resendEnabled = false}}
eval (OtpPrimaryEditTextAction (OtpPrimaryEditText.TextChanged valId newVal)) state = do
  let newState = state { props = state.props { btnActive = if length newVal == 4 then true else false, isValid = false}
                  , data = state.data { otp = if length newVal <= 4 then newVal else state.data.otp }}
  if length newVal >= 4 then do
      _ <- pure $ hideKeyboardOnNavigation true
      exit (GoToHome newState)
      else continue newState
eval (TextChng newVal) state = do
      continue state
eval (StepsHeaderModelAC StepsHeaderModelController.OnArrowClick) state = continueWithCmd state [ do pure $ BackPressed]

eval (AutoFill otpReceived) state = do 
  updateAndExit (state { data {capturedOtp = otpReceived, otp = if (length otpReceived) == 4 then otpReceived else state.data.otp } }) $ GoToHome (state { data {capturedOtp = otpReceived, otp = if (length otpReceived) == 4 then otpReceived else state.data.otp } })
eval (SetToken id )state = do 
  _ <-  pure $ setValueToLocalNativeStore FCM_TOKEN  id
  _ <-  pure $ setValueToLocalStore FCM_TOKEN  id
  continue state
eval _ state = continue state