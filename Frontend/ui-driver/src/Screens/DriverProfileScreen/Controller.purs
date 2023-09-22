{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.DriverProfileScreen.Controller where

import Common.Types.App (CheckBoxOptions)
import Common.Types.App (LazyCheck(..))
import Components.BottomNavBar.Controller as BottomNavBar
import Components.CheckListView as CheckList
import Components.GenericHeader.Controller as GenericHeaderController
import Components.InAppKeyboardModal as InAppKeyboardModal
import Components.PopUpModal.Controller as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryButton as PrimaryButtonController
import Components.PrimaryEditText as PrimaryEditText
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Data.Array ((!!), union, drop, filter, elem, length, foldl, any)
import Data.Int (fromString)
import Data.Lens.Getter ((^.))
import Data.Maybe (fromMaybe, Maybe(..), isJust)
import Data.String as DS
import Data.String.CodeUnits (charAt)
import Debug (spy)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.Commons (getNewIDWithTag,setText)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Engineering.Helpers.LogEvent (logEvent)
import Helpers.Utils (getTime, getCurrentUTC, differenceBetweenTwoUTC, launchAppSettings, generateQR, downloadQR)
import JBridge (firebaseLogEvent, goBackPrevWebPage, toast, showDialer, hideKeyboardOnNavigation, shareImageMessage)
import Language.Strings (getString)
import Language.Types as STR
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import MerchantConfig.Utils (getMerchant, Merchant(..))
import Prelude (class Show, pure, unit, ($), discard, bind, (==), map, not, (/=), (<>), void, (>=), (>), (-), (+), (<=), (||))
import PrestoDOM (Eval, continue, continueWithCmd, exit)
import PrestoDOM.Types.Core (class Loggable, toPropValue)
import PrestoDOM.Utils (updateWithCmdAndExit)
import Screens (ScreenName(..), getScreen)
import Screens.DriverProfileScreen.ScreenData (MenuOptions(..)) as Data
import Screens.DriverProfileScreen.ScreenData (MenuOptions(LIVE_STATS_DASHBOARD), Listtype(..), MenuOptions(..))
import Screens.DriverProfileScreen.Transformer (getAnalyticsData)
import Screens.Types (DriverProfileScreenState, VehicleP, DriverProfileScreenType(..), UpdateType(..), EditRc(..), VehicleDetails(..))
import Screens.Types as ST
import Services.API (GetDriverInfoResp(..), Vehicle(..), DriverProfileSummaryRes(..))
import Services.API as SA
import Services.Accessor (_vehicleColor, _vehicleModel, _certificateNumber)
import Services.Backend (dummyVehicleObject)
import Services.Config (getSupportNumber)
import Storage (setValueToLocalNativeStore, KeyStore(..), getValueToLocalStore)
import Screens.DriverProfileScreen.ScreenData as DriverProfileScreenData
import Screens.SubscriptionScreen.Controller
import Effect.Aff (launchAff_)
import Effect.Uncurried(runEffectFn4)

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen DRIVER_PROFILE_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen DRIVER_PROFILE_SCREEN)
      trackAppEndScreen appId (getScreen DRIVER_PROFILE_SCREEN)
    OptionClick optionIndex -> do
      trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "in_screen" "profile_options_click"
      trackAppEndScreen appId (getScreen DRIVER_PROFILE_SCREEN)
    BottomNavBarAction (BottomNavBar.OnNavigate item) -> do
      trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "bottom_nav_bar" "on_navigate"
      trackAppEndScreen appId (getScreen DRIVER_PROFILE_SCREEN)
    PopUpModalAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_logout" "on_goback"
      PopUpModal.OnButton2Click -> do
        trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_logout" "logout"
        trackAppEndScreen appId (getScreen DRIVER_PROFILE_SCREEN)
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_logout" "no_action"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_logout" "primary_edit_text_changed"
      PopUpModal.CountDown seconds id status timerID -> trackAppScreenEvent appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_logout" "countdown_updated"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_logout" "image_onclick"
      PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_action" "tip_clicked"
      PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen ABOUT_US_SCREEN) "popup_modal_action" "option_with_html_clicked"
      PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_action" "popup_dismissed"
    ActivateOrDeactivateRcPopUpModalAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen VEHICLE_DETAILS_SCREEN) "popup_modal_logout" "on_goback"
      PopUpModal.OnButton2Click -> do
        trackAppActionClick appId (getScreen VEHICLE_DETAILS_SCREEN) "popup_modal_logout" "logout"
        trackAppEndScreen appId (getScreen VEHICLE_DETAILS_SCREEN)
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen VEHICLE_DETAILS_SCREEN) "popup_modal_logout" "no_action"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen VEHICLE_DETAILS_SCREEN) "popup_modal_logout" "primary_edit_text_changed"
      PopUpModal.CountDown seconds id status timerID -> trackAppScreenEvent appId (getScreen VEHICLE_DETAILS_SCREEN) "popup_modal_logout" "countdown_updated"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen VEHICLE_DETAILS_SCREEN) "popup_modal_logout" "image_onclick"
      PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen VEHICLE_DETAILS_SCREEN) "popup_modal_action" "tip_clicked"
      PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen ABOUT_US_SCREEN) "popup_modal_action" "option_with_html_clicked"
      PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen VEHICLE_DETAILS_SCREEN) "popup_modal_action" "popup_dismissed"
    DeleteRcPopUpModalAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen VEHICLE_DETAILS_SCREEN) "popup_modal_logout" "on_goback"
      PopUpModal.OnButton2Click -> do
        trackAppActionClick appId (getScreen VEHICLE_DETAILS_SCREEN) "popup_modal_logout" "logout"
        trackAppEndScreen appId (getScreen VEHICLE_DETAILS_SCREEN)
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen VEHICLE_DETAILS_SCREEN) "popup_modal_logout" "no_action"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen VEHICLE_DETAILS_SCREEN) "popup_modal_logout" "primary_edit_text_changed"
      PopUpModal.CountDown seconds id status timerID -> trackAppScreenEvent appId (getScreen VEHICLE_DETAILS_SCREEN) "popup_modal_logout" "countdown_updated"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen VEHICLE_DETAILS_SCREEN) "popup_modal_logout" "image_onclick"
      PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen ABOUT_US_SCREEN) "popup_modal_action" "option_with_html_clicked"
      PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen VEHICLE_DETAILS_SCREEN) "popup_modal_action" "tip_clicked"
      PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen VEHICLE_DETAILS_SCREEN) "popup_modal_action" "popup_dismissed"
    CallDriverPopUpModalAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_logout" "on_goback"
      PopUpModal.OnButton2Click -> do
        trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_logout" "logout"
        trackAppEndScreen appId (getScreen DRIVER_PROFILE_SCREEN)
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_logout" "no_action"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_logout" "primary_edit_text_changed"
      PopUpModal.CountDown seconds id status timerID -> trackAppScreenEvent appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_logout" "countdown_updated"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_logout" "image_onclick"
      PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_action" "tip_clicked"
      PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen ABOUT_US_SCREEN) "popup_modal_action" "option_with_html_clicked"
      PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_action" "popup_dismissed"
    PrimaryButtonActionController act -> case act of
      PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "primary_button" "go_home_or_submit"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "primary_button" "no_action"
    DriverSummary resp -> trackAppScreenEvent appId (getScreen DRIVER_PROFILE_SCREEN) "in_screen" "get_driver_summary_response"
    GetDriverInfoResponse resp -> trackAppScreenEvent appId (getScreen DRIVER_PROFILE_SCREEN) "in_screen" "get_driver_info_response"
    HideLiveDashboard val -> trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "in_screen" "hide_live_stats_dashboard"
    DriverGenericHeaderAC act -> case act of
      GenericHeaderController.PrefixImgOnClick -> do
        trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "generic_header_action" "back_icon"
        trackAppEndScreen appId (getScreen DRIVER_PROFILE_SCREEN)
      GenericHeaderController.SuffixImgOnClick -> trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "generic_header_action" "forward_icon"
    PrimaryButtonActionController act -> case act of
      PrimaryButton.OnClick -> do
        trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "primary_button" "update_on_click"
        trackAppEndScreen appId (getScreen DRIVER_PROFILE_SCREEN)
      PrimaryButton.NoAction -> trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "primary_button" "update_no_action"
    PrimaryEditTextActionController act -> case act of
      PrimaryEditText.TextChanged valId newVal -> trackAppTextInput appId (getScreen DRIVER_PROFILE_SCREEN) "reenter_dl_number_text_changed" "primary_edit_text"
      PrimaryEditTextController.FocusChanged _ -> trackAppTextInput appId (getScreen DRIVER_PROFILE_SCREEN) "alternate_mobile_number_text_focus_changed" "primary_edit_text"
    NoAction -> trackAppScreenEvent appId (getScreen DRIVER_PROFILE_SCREEN) "in_screen" "no_action"
    RemoveAlternateNumberAC act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "show_delete_popup_modal_action" "delete_account_cancel"
      PopUpModal.OnButton2Click -> do
        trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "show_delete_popup_modal_action" "delete_account_accept"
        trackAppEndScreen appId (getScreen DRIVER_PROFILE_SCREEN)
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "show_delete_popup_modal_action" "no_action"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen DRIVER_PROFILE_SCREEN) "show_delete_popup_modal_action" "image"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen DRIVER_PROFILE_SCREEN) "show_delete_popup_modal_action" "primary_edit_text"
      PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen DRIVER_PROFILE_SCREEN) "show_delete_popup_modal_action" "countdown_updated"
      PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen DRIVER_PROFILE_SCREEN) "show_delete_popup_modal_action" "tip_clicked"
      PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_action" "option_with_html_clicked"
      PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen DRIVER_PROFILE_SCREEN) "show_delete_popup_modal_action" "popup_dismissed"
    InAppKeyboardModalOtp (InAppKeyboardModal.OnSelection key index) -> trackAppActionClick appId (getScreen DRIVER_DETAILS_SCREEN) "in_app_otp_modal" "on_selection"
    InAppKeyboardModalOtp (InAppKeyboardModal.OnClickBack text) -> trackAppActionClick appId (getScreen DRIVER_DETAILS_SCREEN) "in_app_otp_modal" "on_click_back"
    InAppKeyboardModalOtp (InAppKeyboardModal.BackPressed) -> trackAppActionClick appId (getScreen DRIVER_DETAILS_SCREEN) "in_app_otp_modal" "on_backpressed"
    InAppKeyboardModalOtp (InAppKeyboardModal.OnClickDone text) -> trackAppActionClick appId (getScreen DRIVER_DETAILS_SCREEN) "in_app_otp_modal" "on_click_done"
    InAppKeyboardModalOtp (InAppKeyboardModal.OnClickResendOtp) -> trackAppActionClick appId (getScreen DRIVER_DETAILS_SCREEN) "in_app_otp_modal" "on_click_done"
    _ -> pure unit

data ScreenOutput = GoToDriverDetailsScreen DriverProfileScreenState
                    | GoToVehicleDetailsScreen DriverProfileScreenState
                    | GoToBookingOptions DriverProfileScreenState
                    | GoToSelectLanguageScreen DriverProfileScreenState
                    | GoToHelpAndSupportScreen DriverProfileScreenState
                    | GoToDriverHistoryScreen DriverProfileScreenState
                    | ResendAlternateNumberOTP DriverProfileScreenState
                    | VerifyAlternateNumberOTP DriverProfileScreenState
                    | RemoveAlternateNumber DriverProfileScreenState
                    | ValidateAlternateNumber DriverProfileScreenState
                    | UpdateGender DriverProfileScreenState
                    | GoToNotifications
                    | GoToAboutUsScreen
                    | OnBoardingFlow
                    | GoToHomeScreen DriverProfileScreenState
                    | GoToReferralScreen
                    | GoToLogout
                    | GoBack
                    | ActivatingOrDeactivatingRC DriverProfileScreenState
                    | DeletingRc DriverProfileScreenState
                    | CallingDriver DriverProfileScreenState
                    | AddingRC DriverProfileScreenState
                    | UpdateLanguages DriverProfileScreenState (Array String)
                    | SubscriptionScreen

data Action = BackPressed
            | NoAction
            | OptionClick Data.MenuOptions
            | BottomNavBarAction BottomNavBar.Action
            | GetDriverInfoResponse SA.GetDriverInfoResp
            | DriverSummary DriverProfileSummaryRes
            | PopUpModalAction PopUpModal.Action
            | AfterRender
            | HideLiveDashboard String
            | ChangeScreen DriverProfileScreenType
            | GenericHeaderAC GenericHeaderController.Action
            | PrimaryEditTextAC PrimaryEditTextController.Action
            | UpdateValue UpdateType
            | DriverGenericHeaderAC GenericHeaderController.Action
            | PrimaryButtonActionController PrimaryButton.Action
            | PrimaryEditTextActionController PrimaryEditText.Action
            | InAppKeyboardModalOtp InAppKeyboardModal.Action
            | UpdateValueAC PrimaryButton.Action
            | OpenSettings
            | SelectGender
            | UpdateAlternateNumber
            | EditNumberText
            | RemoveAlterNumber
            | RemoveAlternateNumberAC PopUpModal.Action
            | CheckBoxClick ST.Gender
            | LanguageSelection CheckList.Action
            | UpdateButtonClicked PrimaryButton.Action
            | ActivateRc
            | DeactivateRc EditRc
            | CloseEditRc
            | ShowEditRcx
            | ChangeRcDetails Int
            | GetRcsDataResponse SA.GetAllRcDataResp
            | UpdateRC String Boolean
            | ActivateOrDeactivateRcPopUpModalAction PopUpModal.Action
            | PaymentInfoPopUpModalAction PopUpModal.Action
            | DeleteRcPopUpModalAction PopUpModal.Action
            | CallDriverPopUpModalAction PopUpModal.Action
            | AddRc
            | CallDriver
            | CallCustomerSupport
            | OpenRcView Int
            | AddRcButtonAC PrimaryButtonController.Action
            | SkipActiveRc
            | RemoveEditRC
            | DirectActivateRc EditRc
            | UpiQrRendered String
            | DismissQrPopup
            | PayemntInfo
            | LeftKeyAction
            | DownloadQR PrimaryButtonController.Action
            | ShareQR PrimaryButtonController.Action

eval :: Action -> DriverProfileScreenState -> Eval Action ScreenOutput DriverProfileScreenState

eval AfterRender state = continue state

eval (PrimaryEditTextAC (PrimaryEditTextController.TextChanged id val)) state = do
  case state.props.detailsUpdationType of
    Just VEHICLE_AGE -> continue state{props{btnActive = (DS.length val >= 1)}, data{vehicleAge = (fromMaybe 0 (fromString val))}}
    Just VEHICLE_NAME -> continue state{props{btnActive = (DS.length val >= 3)}, data{vehicleName = val}}
    _ -> continue state

eval BackPressed state = if state.props.logoutModalView then continue $ state { props{ logoutModalView = false}}
                                else if state.props.enterOtpModal then continue $ state { props{ enterOtpModal = false}}
                                else if state.props.removeAlternateNumber then continue $ state { props{ removeAlternateNumber = false}}
                                else if state.props.showGenderView then continue $ state { props{ showGenderView = false}}
                                else if state.props.alternateNumberView then if state.data.fromHomeScreen then exit GoBack else continue $ state { props{ alternateNumberView = false},data{driverEditAlternateMobile = Nothing}}
                                else if state.props.alreadyActive then continue $ state {props { alreadyActive = false}}
                                else if state.props.deleteRcView then continue $ state { props{ deleteRcView = false}}
                                else if state.props.activateOrDeactivateRcView then continue $ state { props{ activateOrDeactivateRcView = false}}
                                else if state.props.activateRcView then continue $ state { props{ activateRcView = false}}
                                else if state.props.showLiveDashboard then do
                                continueWithCmd state [do
                                  _ <- pure $ goBackPrevWebPage (getNewIDWithTag "webview")
                                  pure NoAction
                                ]
                                else if state.props.openSettings then continue state{props{openSettings = false}}
                                else if state.props.updateLanguages then continue state{props{updateLanguages = false}}
                                else if isJust state.props.detailsUpdationType then continue state{props{detailsUpdationType = Nothing}}
                                else if state.props.openSettings then continue state{props{openSettings = false}}
                                else exit GoBack


eval (BottomNavBarAction (BottomNavBar.OnNavigate screen)) state = do
  case screen of
    "Home" -> exit $ GoToHomeScreen state
    "Rides" -> exit $ GoToDriverHistoryScreen state
    "Alert" -> do
      _ <- pure $ setValueToLocalNativeStore ALERT_RECEIVED "false"
      let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_alert_click"
      exit $ GoToNotifications
    "Rankings" -> do
      _ <- pure $ setValueToLocalNativeStore REFERRAL_ACTIVATED "false"
      exit $ GoToReferralScreen
    _ -> continue state

eval (UpdateValue value) state = do
  case value of
    LANGUAGE -> continue state {props{updateLanguages = true, detailsUpdationType = Just LANGUAGE}, data {languageList = updateLanguageList state state.data.languagesSpoken}}
    VEHICLE_AGE -> continue state {props{ detailsUpdationType = Just VEHICLE_AGE}}
    VEHICLE_NAME -> continue state{props {detailsUpdationType = Just VEHICLE_NAME}}
    PAYMENT -> continue state{props {upiQrView = true }}
    _ -> continue state

eval (OptionClick optionIndex) state = do
  case optionIndex of
    Data.DRIVER_PRESONAL_DETAILS -> exit $ GoToDriverDetailsScreen state
    Data.DRIVER_VEHICLE_DETAILS -> exit $ GoToVehicleDetailsScreen state
    Data.DRIVER_BANK_DETAILS -> continue state
    Data.DRIVER_BOOKING_OPTIONS -> exit $ GoToBookingOptions state
    Data.MULTI_LANGUAGE -> exit $ GoToSelectLanguageScreen state
    Data.HELP_AND_FAQS -> exit $ GoToHelpAndSupportScreen state
    Data.ABOUT_APP -> exit $ GoToAboutUsScreen
    Data.DRIVER_LOGOUT -> continue $ (state {props = state.props {logoutModalView = true}})
    Data.REFER -> exit $ OnBoardingFlow
    Data.APP_INFO_SETTINGS -> do
      _ <- pure $ launchAppSettings unit
      continue state
    LIVE_STATS_DASHBOARD -> continue state {props {showLiveDashboard = true}}

eval (UpiQrRendered id) state = do
  continueWithCmd state [ do
                    runEffectFn4 generateQR ("upi://pay?pa=" <> state.data.payerVpa) id 200 0
                    pure $ NoAction
                ]
eval (DismissQrPopup) state = continue state {props { upiQrView = false}}

eval (PayemntInfo) state = continue state {props { paymentInfoView = true }}

eval (LeftKeyAction) state = exit $ SubscriptionScreen

eval (DownloadQR (PrimaryButton.OnClick)) state = do 
  _ <- pure $ downloadQR $ getNewIDWithTag "QRpaymentview"
  continueWithCmd state [pure DismissQrPopup]

eval (ShareQR (PrimaryButton.OnClick)) state = do 
  _ <- pure $ shareImageMessage "Hey!\nPay your Namma Yatri fare directly to me ..." (getNewIDWithTag "QRpaymentview")
  continue state

eval (HideLiveDashboard val) state = continue state {props {showLiveDashboard = false}}

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue $ (state {props {logoutModalView = false}})

eval (PopUpModalAction (PopUpModal.OnButton2Click)) state = exit $ GoToLogout

eval (GetDriverInfoResponse (SA.GetDriverInfoResp driverProfileResp)) state = do
  let (SA.Vehicle linkedVehicle) = (fromMaybe dummyVehicleObject driverProfileResp.linkedVehicle)
  continue (state {data = state.data {driverName = driverProfileResp.firstName,
                                      driverVehicleType = linkedVehicle.variant,
                                      driverRating = driverProfileResp.rating,
                                      driverMobile = driverProfileResp.mobileNumber,
                                      vehicleRegNumber = linkedVehicle.registrationNo,
                                      drivingLicenseNo = "",
                                      vehicleModelName = linkedVehicle.model,
                                      vehicleColor = linkedVehicle.color,
                                      vehicleSelected = getDowngradeOptionsSelected  (SA.GetDriverInfoResp driverProfileResp),
                                      driverAlternateNumber = driverProfileResp.alternateNumber ,
                                      driverGender = driverProfileResp.gender,
                                      payerVpa = fromMaybe "" driverProfileResp.payerVpa,
                                      autoPayStatus = getAutopayStatus driverProfileResp.autoPayStatus
                                      }})

eval (GetRcsDataResponse  (SA.GetAllRcDataResp rcDataArray)) state = do
  let rctransformedData = makeRcsTransformData rcDataArray
  if (length rctransformedData == 1)
    then do
      let activeRcVal = rctransformedData
      continue state{data{rcDataArray = rctransformedData, inactiveRCArray = drop 1 rctransformedData, activeRCData = fromMaybe ({ rcStatus : false, rcDetails : {certificateNumber : "", vehicleColor : Nothing, vehicleModel : Nothing}}) (activeRcVal!!0)}}
    else do
      let activeRcVal = filter (\rc -> rc.rcStatus == true) rctransformedData
      if (length activeRcVal == 0)
        then continue state {data{rcDataArray = rctransformedData, inactiveRCArray = drop 1 rctransformedData, activeRCData = fromMaybe ({ rcStatus : false, rcDetails : {certificateNumber : "", vehicleColor : Nothing, vehicleModel : Nothing}}) (rctransformedData!!0) }}
        else
          continue state{data{rcDataArray = rctransformedData, inactiveRCArray = filter (\rc -> rc.rcStatus/=true) $ rctransformedData, activeRCData = fromMaybe ({ rcStatus : false, rcDetails : {certificateNumber : "", vehicleColor : Nothing, vehicleModel : Nothing}}) (activeRcVal!!0)}}

eval (DriverSummary response) state = do
  let (DriverProfileSummaryRes resp) = response
  continue state{data{analyticsData = getAnalyticsData response, languagesSpoken = (fromMaybe [] resp.languagesSpoken), languageList = updateLanguageList state (fromMaybe [] resp.languagesSpoken)}}

eval (ChangeScreen screenType) state = do
  case screenType of
    ST.DRIVER_DETAILS -> do
      let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_profile_personal_click"
      pure unit
    ST.VEHICLE_DETAILS -> do
      let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_profile_vehicle_click"
      pure unit
    _ -> do
      let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_profile_settings_click"
      pure unit
  continue state{props{ screenType = screenType }}

eval OpenSettings state = continue state{props{openSettings = true}}

eval (GenericHeaderAC (GenericHeaderController.PrefixImgOnClick)) state = do
  if state.props.updateLanguages then continue state{props{updateLanguages = false}}
  else if (isJust state.props.detailsUpdationType ) then continue state {props{detailsUpdationType = Nothing}}
  else continue state{ props { openSettings = false }}

eval (DriverGenericHeaderAC(GenericHeaderController.PrefixImgOnClick )) state = if state.data.fromHomeScreen then exit GoBack else continue state {props{showGenderView=false, alternateNumberView=false},data{driverEditAlternateMobile = Nothing}}


eval (PrimaryButtonActionController (PrimaryButton.OnClick)) state = do
  if state.props.alternateNumberView then do
      _ <- pure $ hideKeyboardOnNavigation true
      exit (ValidateAlternateNumber state {props {numberExistError = false, otpIncorrect = false}})
  else exit (UpdateGender state{data{driverGender = state.data.genderTypeSelect},props{showGenderView = false}})

eval SelectGender state = do
  continue state{props{showGenderView = true}, data{genderTypeSelect = if (fromMaybe "" state.data.driverGender) == "UNKNOWN" then Nothing else state.data.driverGender}}

eval UpdateAlternateNumber state = do
  let curr_time = getCurrentUTC ""
  let last_attempt_time = getValueToLocalStore SET_ALTERNATE_TIME
  let time_diff = differenceBetweenTwoUTC curr_time last_attempt_time
  if(time_diff <= 600) then do
    pure $ toast (getString STR.LIMIT_EXCEEDED_FOR_ALTERNATE_NUMBER)
    continue state
  else
    continue state{props{alternateNumberView = true, isEditAlternateMobile = false, mNumberEdtFocused = false}, data{alterNumberEditableText = isJust state.data.driverAlternateNumber}}

eval (PrimaryEditTextActionController (PrimaryEditText.TextChanged id value))state = do
  if DS.length value <= 10 then (do
        let isValidMobileNumber = case (charAt 0 value) of
                              Just a -> if a=='0' || a=='1' || a=='2' || a=='5' then false
                                        else if a=='3' || a=='4' then(
                                              if value=="4000400040" || value=="3000300030" then
                                              true
                                              else false )
                                        else true
                              Nothing -> true
        continue state {data{ driverEditAlternateMobile = Just value}, props = state.props {checkAlternateNumber = isValidMobileNumber,  numberExistError = false}}
  )else continue state

eval (PrimaryEditTextActionController (PrimaryEditTextController.FocusChanged boolean)) state = continue state { props{ mNumberEdtFocused = boolean}}

eval EditNumberText state = do
  let curr_time = getCurrentUTC ""
  let last_attempt_time = getValueToLocalStore SET_ALTERNATE_TIME
  let time_diff = differenceBetweenTwoUTC curr_time last_attempt_time
  if(time_diff <= 600) then do
   pure $ toast (getString STR.LIMIT_EXCEEDED_FOR_ALTERNATE_NUMBER)
   continue state
  else
    continueWithCmd state {data{alterNumberEditableText=false}, props{numberExistError=false, isEditAlternateMobile = true, checkAlternateNumber = true, mNumberEdtFocused = false}}[do
        _ <- pure $ setText (getNewIDWithTag "alternateMobileNumber") (fromMaybe "" state.data.driverAlternateNumber)
        pure NoAction
    ]

eval RemoveAlterNumber state = continue state {props = state.props {removeAlternateNumber = true}}

eval (RemoveAlternateNumberAC (PopUpModal.OnButton1Click)) state = continue state {props{ removeAlternateNumber = false}}

eval (RemoveAlternateNumberAC (PopUpModal.OnButton2Click)) state =
  exit (RemoveAlternateNumber state {props{removeAlternateNumber = false, otpIncorrect = false, alternateNumberView = false, isEditAlternateMobile = false, checkAlternateNumber = true},data{driverEditAlternateMobile = Nothing}})

eval ( CheckBoxClick genderType ) state = do
  continue state{data{genderTypeSelect = getGenderValue genderType}}

eval (InAppKeyboardModalOtp (InAppKeyboardModal.OnClickResendOtp)) state = exit (ResendAlternateNumberOTP state)

eval (InAppKeyboardModalOtp (InAppKeyboardModal.BackPressed)) state =
  continue state { props = state.props {enterOtpModal = false, alternateMobileOtp = "", enterOtpFocusIndex = 0, otpIncorrect = false, otpAttemptsExceeded = false}}

eval (InAppKeyboardModalOtp (InAppKeyboardModal.OnClickBack text)) state = do
  let newVal = (if DS.length( text ) > 0 then (DS.take (DS.length ( text ) - 1 ) text) else "" )
      focusIndex = DS.length newVal
  continue state {props = state.props { alternateMobileOtp = newVal, enterOtpFocusIndex = focusIndex,otpIncorrect = false, otpAttemptsExceeded = false}}

eval (InAppKeyboardModalOtp (InAppKeyboardModal.OnSelection key index)) state = do
  let
    alternateMobileOtp = if (index + 1) > (DS.length state.props.alternateMobileOtp) then ( DS.take 4 (state.props.alternateMobileOtp <> key)) else (DS.take index (state.props.alternateMobileOtp)) <> key <> (DS.take 4 (DS.drop (index+1) state.props.alternateMobileOtp))
    focusIndex = DS.length alternateMobileOtp
  continue state { props = state.props { alternateMobileOtp = alternateMobileOtp, enterOtpFocusIndex = focusIndex, otpIncorrect = false } }

eval (InAppKeyboardModalOtp (InAppKeyboardModal.OnClickDone text)) state = exit (VerifyAlternateNumberOTP state)
eval (LanguageSelection (CheckList.ChangeCheckBoxSate item)) state = do
  let languageChange = map (\ele -> if ele.value == item.value then ele{isSelected = not ele.isSelected} else ele) state.data.languageList
  continue state {data {languageList = languageChange}}

eval (UpdateButtonClicked (PrimaryButton.OnClick)) state = do
  let languagesSelected = getSelectedLanguages state
  pure $ toast $ (getString STR.LANGUAGE_UPDATED)
  exit $ UpdateLanguages state languagesSelected

eval ActivateRc state = continue state{props{activateRcView = true}}

eval CallDriver state =
  continue state{props{callDriver = true}}

eval CallCustomerSupport state = do
  void $ pure $ showDialer (getSupportNumber "") false
  continue state

eval (DeactivateRc rcType) state = do
  if rcType == DEACTIVATING_RC then continue state{props{activateOrDeactivateRcView = true}}
  else if rcType == ACTIVATING_RC then continue state{props{activateOrDeactivateRcView = true}}
  else
    if (length state.data.rcDataArray == 1)
      then do
        pure $ toast $ (getString STR.SINGLE_RC_CANNOT_BE_DELETED)
        continue state
      else
        continue state{props{deleteRcView = true}}

eval (UpdateRC rcNo rcStatus) state  = do
  continue state{props{activateRcView = true}, data{rcNumber = rcNo, isRCActive = rcStatus}}

eval (OpenRcView idx) state  = do
  let val = if elem idx state.data.openInactiveRCViewOrNotArray then filter(\x -> x/=idx) state.data.openInactiveRCViewOrNotArray else state.data.openInactiveRCViewOrNotArray <> [idx]
  continue state{props{openRcView = not state.props.openRcView}, data{openInactiveRCViewOrNotArray = val}}

eval (ActivateOrDeactivateRcPopUpModalAction (PopUpModal.OnButton1Click)) state =   exit $ ActivatingOrDeactivatingRC state

eval (ActivateOrDeactivateRcPopUpModalAction (PopUpModal.OnButton2Click)) state = continue state {props{activateOrDeactivateRcView=false}}

eval (ActivateOrDeactivateRcPopUpModalAction (PopUpModal.DismissPopup)) state = continue state {props{activateOrDeactivateRcView=false}}

eval (PaymentInfoPopUpModalAction (PopUpModal.OnButton1Click)) state = continue state {props{ paymentInfoView = false}}

eval (PaymentInfoPopUpModalAction (PopUpModal.DismissPopup)) state = continue state {props{ paymentInfoView = false}}

eval (DeleteRcPopUpModalAction (PopUpModal.OnButton1Click)) state =  exit $ DeletingRc state

eval (DeleteRcPopUpModalAction (PopUpModal.OnButton2Click)) state = continue state {props{deleteRcView=false}}

eval (CallDriverPopUpModalAction (PopUpModal.OnButton1Click)) state =  do
   exit $ CallingDriver state

eval (CallDriverPopUpModalAction (PopUpModal.OnButton2Click)) state = continue state { props {callDriver = false}}

eval (GenericHeaderAC (GenericHeaderController.PrefixImgOnClick)) state = continue state{ props { screenType = VEHICLE_DETAILS }}

eval (AddRcButtonAC PrimaryButtonController.OnClick) state = do
  exit $ AddingRC state

eval SkipActiveRc state = continue state { props {alreadyActive = false}}

eval RemoveEditRC state = continue state { props {activateRcView = false}}

eval (UpdateValueAC (PrimaryButton.OnClick)) state = do
  if (state.props.detailsUpdationType == Just VEHICLE_AGE) then continue state{props{detailsUpdationType = Nothing}} -- update age
    else if (state.props.detailsUpdationType == Just VEHICLE_NAME) then continue state{props{detailsUpdationType = Nothing}} -- update name
    else continue state

eval (UpdateValueAC (PrimaryButton.OnClick)) state = do
  if (state.props.detailsUpdationType == Just VEHICLE_AGE) then continue state{props{detailsUpdationType = Nothing}} -- update age
    else if (state.props.detailsUpdationType == Just VEHICLE_NAME) then continue state{props{detailsUpdationType = Nothing}} -- update name
    else continue state

eval (DirectActivateRc rcType) state = continueWithCmd state{data{rcNumber = state.data.activeRCData.rcDetails.certificateNumber, isRCActive = state.data.activeRCData.rcStatus}} [
    pure $ DeactivateRc rcType
  ]

eval _ state = continue state

getTitle :: Data.MenuOptions -> String
getTitle menuOption =
  case menuOption of
    Data.DRIVER_PRESONAL_DETAILS -> (getString STR.PERSONAL_DETAILS)
    Data.DRIVER_VEHICLE_DETAILS -> (getString STR.VEHICLE_DETAILS)
    Data.DRIVER_BANK_DETAILS -> (getString STR.BANK_DETAILS)
    Data.MULTI_LANGUAGE -> (getString STR.LANGUAGES)
    Data.HELP_AND_FAQS -> (getString STR.HELP_AND_FAQ)
    Data.ABOUT_APP -> (getString STR.ABOUT)
    Data.REFER -> (getString STR.ADD_YOUR_FRIEND)
    Data.DRIVER_LOGOUT -> (getString STR.LOGOUT)
    Data.APP_INFO_SETTINGS -> (getString STR.APP_INFO)
    Data.LIVE_STATS_DASHBOARD -> (getString STR.LIVE_DASHBOARD)
    Data.DRIVER_BOOKING_OPTIONS -> (getString STR.BOOKING_OPTIONS)

getDowngradeOptionsSelected :: SA.GetDriverInfoResp -> Array VehicleP
getDowngradeOptionsSelected (SA.GetDriverInfoResp driverInfoResponse) =
  [
    {vehicleName: "HATCHBACK", isSelected: driverInfoResponse.canDowngradeToHatchback}
  , {vehicleName: "SEDAN", isSelected: driverInfoResponse.canDowngradeToSedan}
  , {vehicleName: "TAXI" , isSelected: driverInfoResponse.canDowngradeToTaxi}
  ]

getGenderValue :: ST.Gender -> Maybe String
getGenderValue gender =
  case gender of
    ST.MALE -> Just "MALE"
    ST.FEMALE -> Just "FEMALE"
    ST.OTHER -> Just "OTHER"
    _ -> Just "PREFER_NOT_TO_SAY"

checkGenderSelect :: Maybe String -> ST.Gender -> Boolean
checkGenderSelect genderTypeSelect genderType =
  if not (isJust genderTypeSelect) then false
  else do
    let gender = fromMaybe "" genderTypeSelect
    case genderType of
      ST.MALE -> gender == "MALE"
      ST.FEMALE -> gender == "FEMALE"
      ST.PREFER_NOT_TO_SAY -> gender == "PREFER_NOT_TO_SAY"
      ST.OTHER -> gender == "OTHER"

getGenderName :: Maybe String -> Maybe String
getGenderName gender =
  case gender of
    Just value -> case value of
      "MALE" -> Just (getString STR.MALE)
      "FEMALE" -> Just (getString STR.FEMALE)
      "OTHER" -> Just (getString STR.OTHER)
      "PREFER_NOT_TO_SAY" -> Just (getString STR.PREFER_NOT_TO_SAY)
      _ -> Nothing
    Nothing -> Nothing

getSelectedLanguages :: DriverProfileScreenState -> Array String
getSelectedLanguages state =
  let languages = filter (\a -> a.isSelected == true) state.data.languageList
  in  foldl (\acc item -> acc <> [item.value]) [] languages


makeRcsTransformData :: Array SA.GetAllRcDataRecords -> Array VehicleDetails
makeRcsTransformData (listRes) = map (\ (SA.GetAllRcDataRecords rc)-> {
  rcStatus : rc.rcActive,
  rcDetails : {certificateNumber : (rc.rcDetails) ^. _certificateNumber,
  vehicleColor : (rc.rcDetails) ^. _vehicleColor,
  vehicleModel : (rc.rcDetails) ^. _vehicleModel}
  }
  ) listRes

optionList :: String -> Array Listtype
optionList dummy =
    (if (getMerchant FunctionCall /= NAMMAYATRI)  then [{menuOptions: DRIVER_BOOKING_OPTIONS , icon:"ic_booking_options,https://assets.juspay.in/nammayatri/images/driver/ic_booking_options.png"}] else []) <>
    [
      {menuOptions: APP_INFO_SETTINGS , icon:"ny_ic_app_info,https://assets.juspay.in/nammayatri/images/driver/ny_ic_app_info.png"},
      {menuOptions: MULTI_LANGUAGE , icon:"ny_ic_language,https://assets.juspay.in/nammayatri/images/driver/ny_ic_language.png"},
      {menuOptions: HELP_AND_FAQS , icon:"ny_ic_head_phones,https://assets.juspay.in/nammayatri/images/driver/ny_ic_head_phones.png"}
    ]
    <> (if (getMerchant FunctionCall == NAMMAYATRI) then [{menuOptions: LIVE_STATS_DASHBOARD , icon:"ic_graph_black,https://assets.juspay.in/nammayatri/images/common/ic_graph_black.png"}] else []) <>
    [
      {menuOptions: ABOUT_APP , icon:"ny_ic_about,https://assets.juspay.in/nammayatri/images/driver/ny_ic_about.png"},
      {menuOptions: DRIVER_LOGOUT , icon:"ny_ic_logout_grey,https://assets.juspay.in/nammayatri/images/driver/ny_ic_logout_grey.png"}
    ]

updateLanguageList :: DriverProfileScreenState -> Array String -> Array CheckBoxOptions
updateLanguageList state language =
  map (\item -> if (any (_ == item.value)) language
                  then item{isSelected = true}
                  else item{isSelected = false}) (state.data.languageList)
