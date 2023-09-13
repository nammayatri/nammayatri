{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ApplicationStatusScreen.Controller where
import Prelude (class Show, pure, unit, bind, ($), discard, (==), (&&),(||),not,(<=),(>=),(/),(/=),(+))
import PrestoDOM (Eval, continue, exit,continueWithCmd,updateAndExit)
import Screens.Types (ApplicationStatusScreenState)
import PrestoDOM.Types.Core (class Loggable)
import JBridge (openWhatsAppSupport, minimizeApp,toast,showDialer, hideKeyboardOnNavigation )
import Effect.Class (liftEffect)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent,trackAppTextInput)
import Screens (ScreenName(..), getScreen)
import Services.APITypes(DriverRegistrationStatusResp(..))
import Components.PrimaryButton as PrimaryButtonController
import Data.Array (any)
import Components.PopUpModal.Controller as PopUpModal
import Components.ReferralMobileNumber.Controller as ReferralMobileNumberController
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.StepsHeaderModel.Controller as StepsHeaderModelController
import Data.String (length)
import Data.String.CodeUnits (charAt)
import Data.Maybe(Maybe(..))
import Services.Config (getSupportNumber)
import Engineering.Helpers.Commons (getNewIDWithTag,getExpiryTime,setText')
import Storage(KeyStore(..),getValueToLocalStore)
import Language.Strings (getString)
import Language.Types (STR(..))

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen APPLICATION_STATUS_SCREEN)
    BackPressed -> trackAppBackPress appId (getScreen APPLICATION_STATUS_SCREEN)
    PrimaryButtonActionController -> do
      trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "primary_button" "on_click"
      trackAppEndScreen appId (getScreen APPLICATION_STATUS_SCREEN)
    Logout -> do
      trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "in_screen" "Logout"
      trackAppEndScreen appId (getScreen APPLICATION_STATUS_SCREEN)
    SupportCall -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "in_screen" "whats_app_support_call"
    DriverRegistrationStatusAction resp -> trackAppScreenEvent appId (getScreen APPLICATION_STATUS_SCREEN) "in_screen" "driver_registration_status"
    Dummy -> trackAppScreenEvent appId (getScreen APPLICATION_STATUS_SCREEN) "in_screen" "dummy"
    ReTry docType -> case docType of
      "DL" -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "in_screen" "DL_retry_on_click"
      "RC" -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "in_screen" "RC_retry_on_click"
      _ -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "in_screen" "retry_on_click"
    PopUpModalAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "popup_modal" "on_goback"
      PopUpModal.Tipbtnclick _ _ -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "popup_modal" "tip_button_click"
      PopUpModal.DismissPopup -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "popup_modal" "dismiss_popup"
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "popup_modal" "call_support"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "popup_modal_action" "no_action"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "popup_modal_action" "image"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen APPLICATION_STATUS_SCREEN) "popup_modal_action" "primary_edit_text"
      PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen APPLICATION_STATUS_SCREEN) "popup_modal_action" "countdown_updated"
    ExitGoToEnterOtp ->  trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "in_screen" "enter_otp"
    CompleteOnBoardingAction PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "in_screen" "onboardingview"
    CompleteOnBoardingAction PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "in_screen" "na_action"
    AlternateMobileNumberAction act -> case act of
      ReferralMobileNumberController.OnBackClick -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "referral_mobile_number" "on_back_icon_click"
      ReferralMobileNumberController.PrimaryButtonActionController act -> case act of
        PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "referral_mobile_number" "primary_button_on_click"
        PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "referral_mobile_number" "primary_button_no_action"
      ReferralMobileNumberController.PrimaryEditTextActionController act -> case act of
        PrimaryEditTextController.TextChanged valId newVal -> trackAppTextInput appId (getScreen APPLICATION_STATUS_SCREEN) "referral_mobile_number_text_changed" "primary_edit_text"
        PrimaryEditTextController.FocusChanged _ -> trackAppTextInput appId (getScreen APPLICATION_STATUS_SCREEN) "referral_mobile_number_text_focus_changed" "primary_edit_text"
      ReferralMobileNumberController.OnSubTextClick ->  trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "referral_mobile_number" "otpResent"
    StepsHeaderModelAC act -> case act of
      StepsHeaderModelController.OnArrowClick -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "in_screen" "steps_header_on_click"
      StepsHeaderModelController.Logout -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "in_screen" "steps_header_logout"
    PrimaryButtonCompleteRegistrationAC act -> case act of
      PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "primary_button" "onclick"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "primary_button" "no_action"
    PopUpModalLogoutAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "popup_modal" "on_goback"
      PopUpModal.Tipbtnclick _ _ -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "popup_modal" "tip_button_click"
      PopUpModal.DismissPopup -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "popup_modal" "dismiss_popup"
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "popup_modal" "call_support"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "popup_modal_action" "no_action"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen APPLICATION_STATUS_SCREEN) "popup_modal_action" "image"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen APPLICATION_STATUS_SCREEN) "popup_modal_action" "primary_edit_text"
      PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen APPLICATION_STATUS_SCREEN) "popup_modal_action" "countdown_updated"
    RefreshScreen -> trackAppScreenEvent appId (getScreen APPLICATION_STATUS_SCREEN) "in_screen" "Resfresh"
    

data ScreenOutput = GoToHomeScreen | LogoutAccount | GoToDlScreen ApplicationStatusScreenState| GoToVehicleDetailScreen ApplicationStatusScreenState | GoToEnterOtp ApplicationStatusScreenState | AddMobileNumber ApplicationStatusScreenState | ResendOtp ApplicationStatusScreenState | GrantPermissionScreen ApplicationStatusScreenState | Refresh
data Action = BackPressed
              | PrimaryButtonActionController
              | Logout
              | SupportCall
              | Dummy
              | AfterRender
              | DriverRegistrationStatusAction DriverRegistrationStatusResp
              | ReTry String
              | PopUpModalAction PopUpModal.Action
              | CompleteOnBoardingAction PrimaryButtonController.Action
              | AlternateMobileNumberAction ReferralMobileNumberController.Action
              | ExitGoToEnterOtp
              | StepsHeaderModelAC StepsHeaderModelController.Action
              | PrimaryButtonCompleteRegistrationAC PrimaryButtonController.Action
              | PopUpModalLogoutAction PopUpModal.Action
              | RefreshScreen



eval :: Action -> ApplicationStatusScreenState -> Eval Action ScreenOutput ApplicationStatusScreenState
eval AfterRender state =  continue state
eval BackPressed state = do
  _ <- pure $ minimizeApp ""
  continue state
eval (PrimaryButtonActionController) state = exit GoToHomeScreen
eval (CompleteOnBoardingAction PrimaryButtonController.OnClick) state = do
  let timelimit = (((getExpiryTime (getValueToLocalStore INVALID_OTP_TIME) true))/60)
  if state.props.onBoardingFailure then  continue state{props{popupview = true}} else do
    if (not ((timelimit)<=10)||(getValueToLocalStore INVALID_OTP_TIME == "__failed")) then continue state{props{enterMobileNumberView =true,isValidOtp = false,isAlternateMobileNumberExists = false , isValidAlternateNumber = false},data{mobileNumber=""}}
    else continueWithCmd state [do
      _ <- pure $ toast $ (getString LIMIT_EXCEEDED_PLEASE_TRY_AGAIN_AFTER_10MIN)
      pure Dummy
    ]
eval (ReTry docType) state = case docType of
                                "DL" -> exit (GoToDlScreen state)
                                "RC" -> exit (GoToVehicleDetailScreen state)
                                "GP" -> exit (GrantPermissionScreen state)
                                _ -> continue state
eval Logout state = exit LogoutAccount
eval SupportCall  state = continueWithCmd state [do
  _ <- liftEffect $ openWhatsAppSupport "+918618963188"
  pure Dummy
  ]
eval (DriverRegistrationStatusAction (DriverRegistrationStatusResp resp)) state = do
  if (resp.dlVerificationStatus == "VALID" && resp.rcVerificationStatus == "VALID") then
    exit GoToHomeScreen
    else do
      let  popup_visibility = any (_ == resp.dlVerificationStatus) ["FAILED","INVALID"]  || any (_ == resp.rcVerificationStatus) ["FAILED","INVALID"]
      let  onBoardingStatus =  any (_ == resp.dlVerificationStatus) ["LIMIT_EXCEED","NO_DOC_AVAILABLE"] || any (_ == resp.rcVerificationStatus) ["LIMIT_EXCEED","NO_DOC_AVAILABLE"]
      let timeDifference = (getExpiryTime (getValueToLocalStore DOCUMENT_UPLOAD_TIME) true)/3600
      if (timeDifference>=48 && (resp.dlVerificationStatus == "PENDING" || resp.rcVerificationStatus == "PENDING")) then continue state{data { dlVerificationStatus = resp.dlVerificationStatus, rcVerificationStatus = resp.rcVerificationStatus},props{onBoardingFailure = true,isVerificationFailed = false}}
      else continue state { data { dlVerificationStatus = resp.dlVerificationStatus, rcVerificationStatus = resp.rcVerificationStatus}, props{onBoardingFailure = onBoardingStatus, isVerificationFailed = popup_visibility}}
eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue state{props{popupview=false}}
eval (PopUpModalAction (PopUpModal.OnButton2Click)) state = do
  _ <- pure $ showDialer (getSupportNumber "") false -- TODO: FIX_DIALER
  continue state
eval (AlternateMobileNumberAction (ReferralMobileNumberController.OnBackClick)) state = do
  if state.props.enterOtp then do
    continueWithCmd state{props{enterOtp = false, buttonVisibilty = true}} [do
      _ <- setText' (getNewIDWithTag "Referalnumber") state.data.mobileNumber
      pure Dummy
    ]
  else continue state{props{enterMobileNumberView=false}}
eval (AlternateMobileNumberAction (ReferralMobileNumberController.PrimaryButtonActionController (PrimaryButtonController.OnClick))) state = do
  if state.props.enterOtp then exit $ AddMobileNumber state
  else do
    continueWithCmd state [do
      setText' (getNewIDWithTag "Referalnumber") ""
      pure $ ExitGoToEnterOtp
    ]

eval (ExitGoToEnterOtp) state = exit $ GoToEnterOtp state
eval (AlternateMobileNumberAction (ReferralMobileNumberController.PrimaryEditTextActionController (PrimaryEditTextController.TextChanged valId newVal))) state = do
  if state.props.enterOtp then do
    if length newVal == 4 then do
      pure $ hideKeyboardOnNavigation true
      continue state{props{buttonVisibilty = true},data{otpValue = newVal}}
    else continue state{props{buttonVisibilty = false,isValidOtp=false}}
  else do
    _ <- if length newVal >= 10 then do
          pure $ hideKeyboardOnNavigation true
          else pure unit
    let isValidMobileNumber = case (charAt 0 newVal) of
                                      Just a -> if a=='0' || a=='1' || a=='2' || a=='3'|| a=='4' || a=='5' then false
                                      else true
                                      Nothing -> true
    if (length newVal == 10 && isValidMobileNumber) then do
      continue state {props{ buttonVisibilty = if (length newVal == 10 && isValidMobileNumber && not state.props.isAlternateMobileNumberExists) then true else false
                                        ,isValidAlternateNumber =  not isValidMobileNumber},data{mobileNumber = if(length newVal == 10 && isValidMobileNumber) then newVal else ""}
      }
      else continue state {props{isValidAlternateNumber =  not isValidMobileNumber,buttonVisibilty = false,isAlternateMobileNumberExists=false}
      }
eval (AlternateMobileNumberAction (ReferralMobileNumberController.OnSubTextClick)) state = exit $ ResendOtp state

eval (PopUpModalLogoutAction (PopUpModal.OnButton2Click)) state = continue $ (state {props {logoutModalView = false}})

eval (PopUpModalLogoutAction (PopUpModal.OnButton1Click)) state = exit $ LogoutAccount

eval (StepsHeaderModelAC (StepsHeaderModelController.Logout)) state = continue $ (state {props{logoutModalView = true}})

eval RefreshScreen state = exit $ Refresh
eval _ state = continue state

isClickable :: ApplicationStatusScreenState -> String -> Boolean
isClickable state docType = 
       if (docType == "DL") 
           then if (state.data.dlVerificationStatus == "VALID" || state.data.dlVerificationStatus == "PENDING" || state.data.dlVerificationStatus == "LIMIT_EXCEED" ) then false else true
       --else if (docType == "RC")
         --  then if ((state.data.dlVerificationStatus /= "VALID" && state.data.dlVerificationStatus /= "PENDING") || state.data.rcVerificationStatus == "VALID" || state.data.rcVerificationStatus == "PENDING" || state.data.rcVerificationStatus == "LIMIT_EXCEED" ) then false else true
       else if (docType == "GP")
           then if (state.props.isPermissionGranted == false) then true else false-- && state.data.dlVerificationStatus == "VALID" && state.data.rcVerificationStatus == "VALID" ) then true else false
       else 
           true

countPercentage :: Boolean  -> Int
countPercentage val = 
   case val of 
      true -> 33
      _ -> 0

completePercentage :: ApplicationStatusScreenState -> Int
completePercentage state = 
           if (state.data.dlVerificationStatus /= "VALID" && state.data.dlVerificationStatus /= "PENDING") then
                 (countPercentage (state.data.rcVerificationStatus == "VALID" || state.data.rcVerificationStatus == "PENDING")) + (countPercentage (state.props.isPermissionGranted))
           else if (state.data.rcVerificationStatus /= "VALID" && state.data.rcVerificationStatus /= "PENDING") then
                 (countPercentage (state.data.dlVerificationStatus == "VALID" || state.data.dlVerificationStatus == "PENDING")) + (countPercentage (state.props.isPermissionGranted))
           else if (not state.props.isPermissionGranted) then
                 (countPercentage (state.data.dlVerificationStatus == "VALID" || state.data.dlVerificationStatus == "PENDING")) + (countPercentage (state.data.rcVerificationStatus == "VALID" || state.data.rcVerificationStatus == "PENDING"))
           else 100
       