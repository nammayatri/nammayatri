{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.DriverProfileScreen.Controller where

import Components.BottomNavBar.Controller as BottomNavBar
import Components.PopUpModal.Controller as PopUpModal
import Data.Maybe (fromMaybe, Maybe(..), isJust)
import Helpers.Utils (launchAppSettings)
import JBridge (firebaseLogEvent, goBackPrevWebPage, toast)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress)
import Prelude (class Show, pure, unit, ($), discard, bind, (==), map, not, (>=))
import PrestoDOM (Eval, continue, exit, continueWithCmd)
import PrestoDOM.Types.Core (class Loggable)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import Screens.DriverProfileScreen.ScreenData (MenuOptions(..)) as Data
import Screens.Types (DriverProfileScreenState, VehicleP, DriverProfileScreenType(..), UpdateType(..))
import Services.API (GetDriverInfoResp(..), Vehicle(..))
import Services.Backend (dummyVehicleObject)
import Storage (setValueToLocalNativeStore, KeyStore(..), getValueToLocalStore)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Screens.DriverProfileScreen.ScreenData (MenuOptions(LIVE_STATS_DASHBOARD))
import Components.GenericHeader.Controller as GenericHeaderController
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Components.InAppKeyboardModal as InAppKeyboardModal
import JBridge (hideKeyboardOnNavigation)
import Prelude ((>), (-), (+), (<>), (<=), (||), not)
import Data.String (length, take, drop)
import Helpers.Utils (getTime,getCurrentUTC,differenceBetweenTwoUTC)
import Data.String.CodeUnits (charAt)
import Screens.Types as ST
import Components.CheckListView as CheckList
import Common.Types.App (CheckBoxOptions)
import Data.Int (fromString)
import Data.Array (filter)

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen DRIVER_PROFILE_SCREEN)
    BackPressed flag -> do
      trackAppBackPress appId (getScreen DRIVER_PROFILE_SCREEN)
      if flag then trackAppScreenEvent appId (getScreen DRIVER_PROFILE_SCREEN) "in_screen" "backpress_in_logout_modal"
        else trackAppEndScreen appId (getScreen DRIVER_PROFILE_SCREEN)
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
      PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen DRIVER_PROFILE_SCREEN) "popup_modal_action" "popup_dismissed"
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
                    | GoToHomeScreen
                    | GoToReferralScreen
                    | GoToLogout
                    | GoBack

data Action = BackPressed Boolean
            | NoAction
            | OptionClick Data.MenuOptions
            | BottomNavBarAction BottomNavBar.Action
            | GetDriverInfoResponse GetDriverInfoResp
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

eval :: Action -> DriverProfileScreenState -> Eval Action ScreenOutput DriverProfileScreenState

eval AfterRender state = continue state

eval (PrimaryEditTextAC (PrimaryEditTextController.TextChanged id val)) state = do
  case state.props.detailsUpdationType of 
    Just AUTO_AGE -> continue state{props{btnActive = (length val >= 1)}, data{autoAge = (fromMaybe 0 (fromString val))}}
    Just AUTO_NAME -> continue state{props{btnActive = (length val >= 3)}, data{autoName = val}}
    _ -> continue state

eval (BackPressed flag) state = if state.props.logoutModalView then continue $ state { props{ logoutModalView = false}}
                                else if state.props.enterOtpModal then continue $ state { props{ enterOtpModal = false}}
                                else if state.props.removeAlternateNumber then continue $ state { props{ removeAlternateNumber = false}}
                                else if state.props.showGenderView then continue $ state { props{ showGenderView = false}}
                                else if state.props.alternateNumberView then continue $ state { props{ alternateNumberView = false}}
                                else if state.props.showLiveDashboard then do
                                continueWithCmd state [do
                                  _ <- pure $ goBackPrevWebPage (getNewIDWithTag "webview")
                                  pure NoAction
                                ]
                                else if state.props.openSettings then continue state{props{openSettings = false}}
                                else if state.props.updateLanguages then continue state{props{updateLanguages = false}}
                                else if isJust state.props.detailsUpdationType then continue state{props{detailsUpdationType = Nothing}}
                                else exit GoBack

eval (BottomNavBarAction (BottomNavBar.OnNavigate screen)) state = do
  case screen of
    "Home" -> exit $ GoToHomeScreen
    "Rides" -> exit $ GoToDriverHistoryScreen state
    "Alert" -> do
      _ <- pure $ setValueToLocalNativeStore ALERT_RECEIVED "false"
      _ <- pure $ firebaseLogEvent "ny_driver_alert_click"
      exit $ GoToNotifications
    "Rankings" -> do
      _ <- pure $ setValueToLocalNativeStore REFERRAL_ACTIVATED "false"
      exit $ GoToReferralScreen
    _ -> continue state

eval (UpdateValue value) state = do 
  case value of 
    LANGUAGE -> continue state {props{updateLanguages = true, detailsUpdationType = Just LANGUAGE}}
    AUTO_AGE -> continue state {props{ detailsUpdationType = Just AUTO_AGE}}
    AUTO_NAME -> continue state{props {detailsUpdationType = Just AUTO_NAME}}
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

eval (HideLiveDashboard val) state = continue state {props {showLiveDashboard = false}}

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue $ (state {props {logoutModalView = false}})

eval (PopUpModalAction (PopUpModal.OnButton2Click)) state = exit $ GoToLogout

eval (GetDriverInfoResponse (GetDriverInfoResp driverProfileResp)) state = do
  let (Vehicle linkedVehicle) = (fromMaybe dummyVehicleObject driverProfileResp.linkedVehicle)
  continue (state {data = state.data {driverName = driverProfileResp.firstName,
                                      driverVehicleType = linkedVehicle.variant,
                                      driverRating = driverProfileResp.rating,
                                      driverMobile = driverProfileResp.mobileNumber,
                                      vehicleRegNumber = linkedVehicle.registrationNo,
                                      drivingLicenseNo = "",
                                      vehicleModelName = linkedVehicle.model,
                                      vehicleColor = linkedVehicle.color,
                                      vehicleSelected = getDowngradeOptionsSelected  (GetDriverInfoResp driverProfileResp),
                                      driverAlternateNumber = driverProfileResp.alternateNumber ,
                                      driverGender = driverProfileResp.gender
                                      }})


eval (ChangeScreen screenType) state = continue state{props{ screenType = screenType }}

eval OpenSettings state = continue state{props{openSettings = true}}

eval (GenericHeaderAC (GenericHeaderController.PrefixImgOnClick)) state = do 
  if state.props.updateLanguages then continue state{props{updateLanguages = false}}
  else if (isJust state.props.detailsUpdationType ) then continue state {props{detailsUpdationType = Nothing}}
  else continue state{ props { openSettings = false }}

eval (DriverGenericHeaderAC(GenericHeaderController.PrefixImgOnClick )) state = continue state {props{showGenderView=false, alternateNumberView=false}}


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
    pure $ toast (getString LIMIT_EXCEEDED_FOR_ALTERNATE_NUMBER)
    continue state
  else 
    continue state{props{alternateNumberView = true, isEditAlternateMobile = false, mNumberEdtFocused = false}, data{alterNumberEditableText = isJust state.data.driverAlternateNumber}}

eval (PrimaryEditTextActionController (PrimaryEditText.TextChanged id value))state = do
  if length value <= 10 then (do
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
   pure $ toast (getString LIMIT_EXCEEDED_FOR_ALTERNATE_NUMBER)
   continue state
  else
    continue state {data{alterNumberEditableText=false}, props{numberExistError=false, isEditAlternateMobile = true, checkAlternateNumber = true, mNumberEdtFocused = false}}

eval RemoveAlterNumber state = continue state {props = state.props {removeAlternateNumber = true}}

eval (RemoveAlternateNumberAC (PopUpModal.OnButton1Click)) state = continue state {props{ removeAlternateNumber = false}}

eval (RemoveAlternateNumberAC (PopUpModal.OnButton2Click)) state = 
  exit (RemoveAlternateNumber state {props{removeAlternateNumber = false, otpIncorrect = false, alternateNumberView = false, isEditAlternateMobile = false, checkAlternateNumber = true}})

eval ( CheckBoxClick genderType ) state = do
  continue state{data{genderTypeSelect = getGenderValue genderType}}  

eval (InAppKeyboardModalOtp (InAppKeyboardModal.OnClickResendOtp)) state = exit (ResendAlternateNumberOTP state)

eval (InAppKeyboardModalOtp (InAppKeyboardModal.BackPressed)) state = 
  continue state { props = state.props {enterOtpModal = false, alternateMobileOtp = "", enterOtpFocusIndex = 0, otpIncorrect = false, otpAttemptsExceeded = false}}

eval (InAppKeyboardModalOtp (InAppKeyboardModal.OnClickBack text)) state = do
  let newVal = (if length( text ) > 0 then (take (length ( text ) - 1 ) text) else "" )
      focusIndex = length newVal
  continue state {props = state.props { alternateMobileOtp = newVal, enterOtpFocusIndex = focusIndex,otpIncorrect = false, otpAttemptsExceeded = false}}

eval (InAppKeyboardModalOtp (InAppKeyboardModal.OnSelection key index)) state = do
  let
    alternateMobileOtp = if (index + 1) > (length state.props.alternateMobileOtp) then ( take 4 (state.props.alternateMobileOtp <> key)) else (take index (state.props.alternateMobileOtp)) <> key <> (take 4 (drop (index+1) state.props.alternateMobileOtp))
    focusIndex = length alternateMobileOtp
  continue state { props = state.props { alternateMobileOtp = alternateMobileOtp, enterOtpFocusIndex = focusIndex, otpIncorrect = false } }

eval (InAppKeyboardModalOtp (InAppKeyboardModal.OnClickDone text)) state = exit (VerifyAlternateNumberOTP state)
eval (LanguageSelection (CheckList.ChangeCheckBoxSate item)) state = do
  let languageChange = map (\ele -> if ele.value == item.value then ele{isSelected = not ele.isSelected} else ele) state.data.languageList 
  continue state {data {languageList = languageChange}}

eval (UpdateButtonClicked (PrimaryButton.OnClick)) state = do
  let languagesSelected = getSelectedLanguages state
  continue state

eval (UpdateValueAC (PrimaryButton.OnClick)) state = do 
  if (state.props.detailsUpdationType == Just AUTO_AGE) then continue state{props{detailsUpdationType = Nothing}} -- update age 
    else if (state.props.detailsUpdationType == Just AUTO_NAME) then continue state{props{detailsUpdationType = Nothing}} -- update name 
    else continue state

eval _ state = continue state

getTitle :: Data.MenuOptions -> String
getTitle menuOption =
  case menuOption of
    Data.DRIVER_PRESONAL_DETAILS -> (getString PERSONAL_DETAILS)
    Data.DRIVER_VEHICLE_DETAILS -> (getString VEHICLE_DETAILS)
    Data.DRIVER_BANK_DETAILS -> (getString BANK_DETAILS)
    Data.MULTI_LANGUAGE -> (getString LANGUAGES)
    Data.HELP_AND_FAQS -> (getString HELP_AND_FAQ)
    Data.ABOUT_APP -> (getString ABOUT)
    Data.REFER -> (getString ADD_YOUR_FRIEND)
    Data.DRIVER_LOGOUT -> (getString LOGOUT)
    Data.APP_INFO_SETTINGS -> (getString APP_INFO)
    Data.LIVE_STATS_DASHBOARD -> (getString LIVE_DASHBOARD)
    Data.DRIVER_BOOKING_OPTIONS -> (getString BOOKING_OPTIONS)

getDowngradeOptionsSelected :: GetDriverInfoResp -> Array VehicleP
getDowngradeOptionsSelected (GetDriverInfoResp driverInfoResponse) =
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
      "MALE" -> Just (getString MALE)
      "FEMALE" -> Just (getString FEMALE)
      "OTHER" -> Just (getString OTHER)
      "PREFER_NOT_TO_SAY" -> Just (getString PREFER_NOT_TO_SAY)
      _ -> Nothing
    Nothing -> Nothing
getSelectedLanguages :: DriverProfileScreenState -> Array CheckBoxOptions
getSelectedLanguages state = do
  let languages = filter (\a -> a.isSelected == true) state.data.languageList
  languages
