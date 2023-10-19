{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AadhaarVerificationScreen.Controller where

import Debug

import Components.PopUpModal as PopUpModal
import Components.PrimaryButton.Controller as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), length, replace, replaceAll)
import Data.String.CodeUnits (charAt)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (getNewIDWithTag, setText)
import JBridge (requestKeyboardShow, hideKeyboardOnNavigation, minimizeApp)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Prelude (class Show, bind, discard, not, pure, unit, when, ($), (&&), (<=), (==), (>), (||), (<>), (+), show)
import PrestoDOM (Eval, continue, continueWithCmd, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.AddVehicleDetailsScreen.Controller (dateFormat)
import Screens.Types (AadhaarStage(..), AadhaarVerificationScreenState)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen AADHAAR_VERIFICATION_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen AADHAAR_VERIFICATION_SCREEN)
      trackAppEndScreen appId (getScreen AADHAAR_VERIFICATION_SCREEN)
    AadhaarNumberEditText act -> case act of
      PrimaryEditText.TextChanged _ _ -> trackAppTextInput appId (getScreen AADHAAR_VERIFICATION_SCREEN) "mobile_number_text_changed" "primary_edit_text"
      PrimaryEditText.FocusChanged _ -> trackAppTextInput appId (getScreen AADHAAR_VERIFICATION_SCREEN) "mobile_number_focus_changed" "primary_edit_text"
    AadhaarOtpEditText act -> case act of
      PrimaryEditText.TextChanged _ _ -> trackAppTextInput appId (getScreen AADHAAR_VERIFICATION_SCREEN) "mobile_number_text_changed" "primary_edit_text"
      PrimaryEditText.FocusChanged _ -> trackAppTextInput appId (getScreen AADHAAR_VERIFICATION_SCREEN) "mobile_number_focus_changed" "primary_edit_text"
    AadhaarNameEditText act -> case act of
      PrimaryEditText.TextChanged _ _ -> trackAppTextInput appId (getScreen AADHAAR_VERIFICATION_SCREEN) "mobile_number_text_changed" "primary_edit_text"
      PrimaryEditText.FocusChanged _ -> trackAppTextInput appId (getScreen AADHAAR_VERIFICATION_SCREEN) "mobile_number_focus_changed" "primary_edit_text"
    AadhaarGenderEditText act -> case act of
      PrimaryEditText.TextChanged _ _ -> trackAppTextInput appId (getScreen AADHAAR_VERIFICATION_SCREEN) "mobile_number_text_changed" "primary_edit_text"
      PrimaryEditText.FocusChanged _ -> trackAppTextInput appId (getScreen AADHAAR_VERIFICATION_SCREEN) "mobile_number_focus_changed" "primary_edit_text"
    PrimaryButtonAC act -> case act of
      PrimaryButton.OnClick -> do
        trackAppActionClick appId (getScreen AADHAAR_VERIFICATION_SCREEN) "primary_button" "next_on_click"
        trackAppEndScreen appId (getScreen AADHAAR_VERIFICATION_SCREEN)
      PrimaryButton.NoAction -> trackAppActionClick appId (getScreen AADHAAR_VERIFICATION_SCREEN) "primary_button" "no_action"
    ResendOTP -> trackAppActionClick appId (getScreen AADHAAR_VERIFICATION_SCREEN) "in_screen" "resend"
    ResendTimer sec -> trackAppScreenEvent appId (getScreen AADHAAR_VERIFICATION_SCREEN) "in_screen" "timer_action"
    Logout -> trackAppEndScreen appId (getScreen AADHAAR_VERIFICATION_SCREEN)
    PopUpModalAC _ -> pure unit
    SelectDateOfBirthAction -> pure unit
    DatePicker _ _ _ _ _-> pure unit

data ScreenOutput = GoToOtpStage AadhaarVerificationScreenState
  | VerfiyOTP AadhaarVerificationScreenState
  | ResendAadhaarOTP AadhaarVerificationScreenState
  | GoToHomeScreen AadhaarVerificationScreenState
  | LogOut AadhaarVerificationScreenState
  | UnVerifiedAadhaarData AadhaarVerificationScreenState
  | GoBack

data Action = BackPressed
            | AadhaarNumberEditText PrimaryEditText.Action
            | AadhaarOtpEditText PrimaryEditText.Action
            | AadhaarNameEditText PrimaryEditText.Action
            | AadhaarGenderEditText PrimaryEditText.Action
            | PrimaryButtonAC PrimaryButton.Action
            | ResendOTP
            | AfterRender
            | ResendTimer String
            | Logout
            | PopUpModalAC PopUpModal.Action
            | SelectDateOfBirthAction
            | DatePicker String String Int Int Int

eval :: Action -> AadhaarVerificationScreenState -> Eval Action ScreenOutput AadhaarVerificationScreenState
eval action state = case action of
  AfterRender -> continue state
  BackPressed ->  if state.props.currentStage == VerifyAadhaar then continue state{props{currentStage = EnterAadhaar}}
                  else exit $ GoBack

  (PrimaryButtonAC (PrimaryButton.OnClick)) ->
    case state.props.currentStage of
      EnterAadhaar -> do
        pure $ setText (getNewIDWithTag "EnterAadhaarOTPEditText") ""
        exit $ GoToOtpStage state
      VerifyAadhaar -> exit $ VerfiyOTP state
      AadhaarDetails -> exit $ UnVerifiedAadhaarData state
  (AadhaarNumberEditText (PrimaryEditText.TextChanged _ newVal)) -> do
    let aadhaarNumber = (replaceAll (Pattern " ") (Replacement "") newVal)
    let len = length aadhaarNumber
    pure $ hideKeyboardOnNavigation (len == 12)
    continue state { props = state.props { btnActive = len == 12}, data = state.data { aadhaarNumber = if len <= 12 then aadhaarNumber else state.data.aadhaarNumber}}
  (AadhaarOtpEditText (PrimaryEditText.TextChanged _ newVal)) -> do
    let otp = (replace (Pattern " ") (Replacement "") newVal)
    let len = length otp
    pure $ hideKeyboardOnNavigation (len == 6)
    continue state { props = state.props { btnActive = len == 6}, data = state.data { otp = if len <= 6 then otp else state.data.otp}}
  ResendTimer time -> do
    if time == "EXPIRED" then
      continue state{props{resendEnabled = true}, data{timer ="60s"}}
      else continue state{data{timer = time}}
  ResendOTP -> exit $ ResendAadhaarOTP state {props{resendEnabled = false}}
  Logout -> do
    pure $ hideKeyboardOnNavigation true
    continue state{props{showLogoutPopup = true}}
  PopUpModalAC (PopUpModal.PrimaryButton1 PrimaryButton.OnClick) -> continue $ (state {props {showLogoutPopup = false}})
  PopUpModalAC (PopUpModal.PrimaryButton2 PrimaryButton.OnClick) -> exit $ LogOut state

  DatePicker _ resp year month date -> do 
    case resp of 
      "SELECTED" -> continue state {data { driverDob = (show date) <> "/" <> (show (month+1)) <> "/" <> (show year)}
                                    , props {isDateClickable = true}}
      _ -> continue state {props {isDateClickable = true}}

  SelectDateOfBirthAction -> continue state {props {isDateClickable = false}}

  AadhaarNameEditText (PrimaryEditText.TextChanged _ val) -> continue state {data { driverName =  val }}

  AadhaarGenderEditText (PrimaryEditText.TextChanged _ val) -> continue state {data { driverGender =  val }}

  _ -> continue state