{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.NammaSafetyScreen.Controller where

import Components.GenericHeader.Controller as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.StepsHeaderModel.Controller as StepsHeaderModelController
import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.String.CodeUnits (charAt)
import Debug (spy)
import Engineering.Helpers.Commons (getNewIDWithTag, os)
import JBridge (hideKeyboardOnNavigation, toast, toggleBtnLoader, minimizeApp, firebaseLogEvent)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog, trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Prelude (class Show, bind, pure, unit, show, ($), (&&), (-), (<=), (==), (>), (||), discard, void)
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit, LetterSpacing(..))
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (NammaSafetyScreenState, Stage(..))
import Storage (KeyStore(..), setValueToLocalNativeStore)
import Common.Types.App (LazyCheck(..)) as Lazy

instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = case action of
        AfterRender -> trackAppScreenRender appId "screen" (getScreen NAMMASAFETY_ONBOARDING_SCREEN)
        StepsHeaderModelAC _ -> trackAppActionClick appId (getScreen NAMMASAFETY_ONBOARDING_SCREEN) "steps_header_modal" "backpressed"
        BackPressed -> trackAppBackPress appId (getScreen HOME_SCREEN)
        GenericHeaderAC act -> case act of 
          GenericHeaderController.PrefixImgOnClick -> do 
            _ <- pure $ spy "calll log" "" 
            trackAppActionClick appId (getScreen NAMMASAFETY_ONBOARDING_SCREEN) "generic_header_action" "back_icon"
            trackAppEndScreen appId (getScreen NAMMASAFETY_ONBOARDING_SCREEN)
          GenericHeaderController.SuffixImgOnClick -> trackAppActionClick appId (getScreen NAMMASAFETY_ONBOARDING_SCREEN) "generic_header_action" "forward_icon"
        StartNammaSafetyOnboarding act -> case act of
          PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen NAMMASAFETY_ONBOARDING_SCREEN) "start_onboarding" "primary button"
          PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen NAMMASAFETY_ONBOARDING_SCREEN) "no_action" "primary button"
        GoToNextStep act -> case act of
          PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen NAMMASAFETY_ONBOARDING_SCREEN) "next_step_onboard" "primary button"
          PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen NAMMASAFETY_ONBOARDING_SCREEN) "no_action" "primary button"
        _ -> trackAppScreenRender appId "screen" (getScreen NAMMASAFETY_ONBOARDING_SCREEN)
        

data ScreenOutput = GoBack

data Action = BackPressed
             | AfterRender
             | StepsHeaderModelAC StepsHeaderModelController.Action
             | GenericHeaderAC GenericHeaderController.Action
             | GenericHeaderACEdu GenericHeaderController.Action
             | StartNammaSafetyOnboarding PrimaryButtonController.Action
             | GoToNextStep PrimaryButtonController.Action
             | SkipToNextStep PrimaryButtonController.Action
             | EditEmergencyContacts PrimaryButtonController.Action
             | GoToEducation Stage
             | ShowAboutNammaSafety
             

eval :: Action -> NammaSafetyScreenState -> Eval Action ScreenOutput NammaSafetyScreenState


eval (StepsHeaderModelAC StepsHeaderModelController.OnArrowClick) state = continueWithCmd state [ do pure $ BackPressed ]

eval (GenericHeaderAC (GenericHeaderController.PrefixImgOnClick)) state = do
  _ <- pure $ spy "calll1" ""
  exit $ GoBack

eval (GenericHeaderACEdu (GenericHeaderController.PrefixImgOnClick)) state = continue state{ props {currentStage = AboutNammaSafety}}

eval AfterRender state = continue state

eval (StartNammaSafetyOnboarding PrimaryButtonController.OnClick) state = continue state {props {currentStage = SetTriggerCustomerSupport}}

eval (EditEmergencyContacts PrimaryButtonController.OnClick) state = continue state {props {currentStage = SetTriggerCustomerSupport}}

eval (GoToEducation stage) state = continue state {props {currentStage = stage}}

eval (ShowAboutNammaSafety) state = continue state {props {currentStage = AboutNammaSafety, showOnboarding = false}}

eval (GoToNextStep PrimaryButtonController.OnClick) state = do
  _ <- pure $ spy "currentStage" state.props.currentStage
  case state.props.currentStage of
    SetTriggerCustomerSupport ->  continue state {props {currentStage = SetNightTimeSafetyAlert}}
    SetNightTimeSafetyAlert ->  continue state {props {currentStage = SetDefaultEmergencyContacts}}
    SetDefaultEmergencyContacts ->  continue state {props {currentStage = SetPersonalSafetySettings}}
    SetPersonalSafetySettings -> continue state{props {currentStage = NammaSafetyDashboard, showOnboarding = false}}
    _ -> continue state

eval (SkipToNextStep PrimaryButtonController.OnClick) state = do
  _ <- pure $ spy "currentStage" state.props.currentStage
  case state.props.currentStage of
    SetTriggerCustomerSupport ->  continue state {props {currentStage = SetNightTimeSafetyAlert}}
    SetNightTimeSafetyAlert ->  continue state {props {currentStage = SetDefaultEmergencyContacts}}
    SetDefaultEmergencyContacts ->  continue state {props {currentStage = SetPersonalSafetySettings}}
    SetPersonalSafetySettings -> continue state
    _ -> continue state

eval _ state = continue state

-- eval (VerifyOTPButtonAction PrimaryButtonController.OnClick) state = do
--     _ <- pure $ hideKeyboardOnNavigation true
--     _ <- pure $ clearCountDownTimer state.data.timerID
--     updateAndExit state $ GoToAccountSetUp state

-- eval (MobileNumberEditTextAction (PrimaryEditTextController.TextChanged id value)) state = do
--     _ <- if length value == 10 then do
--             pure $ hideKeyboardOnNavigation true
--             else pure unit
--     let isValidMobileNumber = case (charAt 0 value) of
--                                     Just a -> if a=='0' || a=='1' || a=='2' || a=='3' || a=='4' then false
--                                                 else if a=='5' then
--                                                     if value=="5000500050" then true else false
--                                                         else true
--                                     Nothing -> true
--     if (length value == 10 && isValidMobileNumber) then do
--         _ <- pure $ firebaseLogEvent "ny_user_mobnum_entry"
--         pure unit
--         else pure unit
--     let newState = state { props = state.props { isValidMobileNumber = isValidMobileNumber
--                                         , btnActiveMobileNumber = if (length value == 10 && isValidMobileNumber) then true else false}
--                                         , data = state.data { mobileNumber = if length value <= 10 then value else state.data.mobileNumber}}
--     continue newState

-- eval (MobileNumberEditTextAction (PrimaryEditTextController.FocusChanged boolean)) state = continue state { props{ mNumberEdtFocused = boolean}}

-- eval (OTPEditTextAction (PrimaryEditTextController.FocusChanged boolean)) state = continue state { props{ otpEdtFocused = boolean}}

-- eval (OTPEditTextAction (PrimaryEditTextController.TextChanged id value)) state = do
--     let newState = state { props = state.props { btnActiveOTP = if length value == 4 then true else false, letterSpacing = PX if value == "" then 1.0 else 6.0, wrongOTP = if state.props.wrongOTP && value == "" then true else false}
--                   , data = state.data { otp = if length value <= 4 then value else state.data.otp }}
--     if length value == 4 then do
--         pure $ hideKeyboardOnNavigation true
--         _ <- pure $ clearCountDownTimer state.data.timerID
--         updateAndExit newState $ GoToAccountSetUp newState
--     else
--         continue newState

-- eval (GenericHeaderActionController (GenericHeaderController.PrefixImgOnClick )) state = continueWithCmd state [ do pure $ BackPressed ]

-- eval Resend state = do
--     let newState = state {data{attempts = if (state.data.attempts > 0) then state.data.attempts - 1 else state.data.attempts},props{resendEnable = false}}
--     if state.data.attempts == 0 then do
--         _ <- pure $ toast (getString OTP_ENTERING_LIMIT_EXHAUSTED_PLEASE_TRY_AGAIN_LATER)
--         _ <- pure $ toggleBtnLoader "" false
--         continue newState{props{enterOTP = false}}
--       else do 
--         _ <- pure $ toast (getString OTP_RESENT_SUCCESSFULLY)
--         exit $ ResendOTP newState

-- eval (AutoFill otp) state = do
--     _ <- pure $ firebaseLogEvent "ny_user_otp_autoread"
--     let newState = state {props = state.props {isReadingOTP = if length otp == 4 then false else true ,capturedOtp = otp}, data = state.data { otp = if (length otp <= 4) then otp else state.data.otp}}
--     updateAndExit newState $ GoToAccountSetUp (newState)
   
-- eval (BackPressed flag) state = do
--       _ <- pure $ printLog "state" state
--       _ <- pure $ toggleBtnLoader "" false
--       _ <- pure $ clearCountDownTimer state.data.timerID
--       let newState = state {props{enterOTP =  false,letterSpacing = PX 1.0},data{otp = ""}}
--       _ <- pure $ hideKeyboardOnNavigation true
--       if state.props.enterOTP then exit $ GoBack newState
--         else if MU.showCarouselScreen Lazy.FunctionCall then exit $ GoToWelcomeScreen newState{data{mobileNumber = ""}} 
--             else do 
--                 void $ pure $ minimizeApp ""
--                 continue state

-- eval (CountDown seconds id status timerID) state = do
--         _ <- pure $ printLog "timer" $ show seconds
--         if status == "EXPIRED" then do
--             _ <- pure $ clearCountDownTimer state.data.timerID
--             let newState = state{data{timer = 30, timerID = ""},props = state.props{resendEnable = true}}
--             continue newState
--         else
--             continue $ state{data{timer = seconds, timerID=timerID},props = state.props{resendEnable = false}}
-- eval (SetToken id )state = do
--   _ <- pure $ spy "SetTokenSetToken" id
--   _ <- pure $ setValueToLocalNativeStore FCM_TOKEN  id
--   continue state

-- eval (SetPhoneNumber number )state = continue state {props { editTextVal = number, mNumberEdtFocused = true}}

-- eval ContinueCommand state = exit $ GoToOTP state{data{timer = 30, timerID = ""},props = state.props{btnActiveOTP = false, resendEnable = false}}

