{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverDetailsScreen.Controller where

import Common.Types.App (LazyCheck(..))
import Common.Types.App (OptionButtonList)
import Components.InAppKeyboardModal as InAppKeyboardModal
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButtonController
import Components.SelectListModal as SelectListModal
import Data.Array as Array
import Data.Function.Uncurried (runFn2)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (take, length, drop)
import Data.String.CodeUnits (charAt)
import Debug (spy)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (getNewIDWithTag, getCurrentUTC)
import Helpers.Utils (getGenderIndex)
import JBridge (renderBase64Image, toast, differenceBetweenTwoUTC)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (class Show, unit, (/=), ($), (<>), (>), (-), (==), (||), (<=), (+), (<), (<=), pure, bind, discard, (&&), show, not)
import PrestoDOM (Eval, continue, exit, continueWithCmd)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.DriverDetailsScreen.ComponentConfig (ListOptions(..))
import Screens.Types (DriverDetailsScreenState, KeyboardModalType(..))
import Storage (KeyStore(..), getValueToLocalStore)
import Types.App (GlobalState(..), DRIVER_DETAILS_SCREEN_OUTPUT(..), FlowBT, ScreenType(..))




instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen DRIVER_DETAILS_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen DRIVER_DETAILS_SCREEN)
      trackAppEndScreen appId (getScreen DRIVER_DETAILS_SCREEN)
    CallBackImageUpload str imageName imagePath -> trackAppScreenEvent appId (getScreen DRIVER_DETAILS_SCREEN) "in_screen" "upload_callback_image"
    RenderBase64Image -> trackAppScreenEvent appId (getScreen DRIVER_DETAILS_SCREEN) "in_screen" "render_base_image"
    UploadFileAction -> trackAppActionClick appId (getScreen DRIVER_DETAILS_SCREEN) "in_screen" "upload_file"
    NoAction -> trackAppScreenEvent appId (getScreen DRIVER_DETAILS_SCREEN) "in_screen" "no_action"
    ClickAddAlternateButton  -> trackAppActionClick appId (getScreen DRIVER_DETAILS_SCREEN) "in_screen" "on_click_add_alternate"
    ClickEditAlternateNumber  -> trackAppActionClick appId (getScreen DRIVER_DETAILS_SCREEN) "in_screen" "on_click_edit-alternate"
    ClickRemoveAlternateNumber -> trackAppActionClick appId (getScreen DRIVER_DETAILS_SCREEN) "in_screen" "on_click_remove_alternate"
    InAppKeyboardModalMobile (InAppKeyboardModal.OnSelection key index) -> trackAppActionClick appId (getScreen DRIVER_DETAILS_SCREEN) "in_app_keyboard_mobile_modal" "on_selection"
    InAppKeyboardModalMobile (InAppKeyboardModal.OnClickBack text) -> trackAppActionClick appId (getScreen DRIVER_DETAILS_SCREEN) "in_app_keyboard_mobile_modal" "on_click_back"
    InAppKeyboardModalMobile (InAppKeyboardModal.BackPressed) -> trackAppActionClick appId (getScreen DRIVER_DETAILS_SCREEN) "in_app_keyboard_mobile_modal" "on_backpressed"
    InAppKeyboardModalMobile (InAppKeyboardModal.OnClickDone text) -> trackAppActionClick appId (getScreen DRIVER_DETAILS_SCREEN) "in_app_keyboard_mobile_modal" "on_click_done"
    InAppKeyboardModalOtp (InAppKeyboardModal.OnSelection key index) -> trackAppActionClick appId (getScreen DRIVER_DETAILS_SCREEN) "in_app_otp_modal" "on_selection"
    InAppKeyboardModalOtp (InAppKeyboardModal.OnClickBack text) -> trackAppActionClick appId (getScreen DRIVER_DETAILS_SCREEN) "in_app_otp_modal" "on_click_back"
    InAppKeyboardModalOtp (InAppKeyboardModal.BackPressed) -> trackAppActionClick appId (getScreen DRIVER_DETAILS_SCREEN) "in_app_otp_modal" "on_backpressed"
    InAppKeyboardModalOtp (InAppKeyboardModal.OnClickDone text) -> trackAppActionClick appId (getScreen DRIVER_DETAILS_SCREEN) "in_app_otp_modal" "on_click_done"
    PopUpModalAction (PopUpModal.OnButton1Click) -> trackAppActionClick appId (getScreen DRIVER_DETAILS_SCREEN) "pop_up_modal" "On_click_button1"
    PopUpModalAction (PopUpModal.OnButton2Click) -> trackAppActionClick appId (getScreen DRIVER_DETAILS_SCREEN) "pop_up_modal" "On_click_button2"
    PopUpModalActions (PopUpModal.OnButton2Click) -> trackAppActionClick appId (getScreen DRIVER_DETAILS_SCREEN) "pop_up_modal2" "On_click_button2"
    InAppKeyboardModalOtp (InAppKeyboardModal.OnClickResendOtp) -> trackAppActionClick appId (getScreen DRIVER_DETAILS_SCREEN) "in_app_otp_modal" "on_click_done"
    _ -> trackAppActionClick appId (getScreen DRIVER_DETAILS_SCREEN) "in_app_otp_modal" "on_click_done"



data ScreenOutput = GoBack DriverDetailsScreenState
                    | ValidateAlternateNumber DriverDetailsScreenState
                    | ResendAlternateNumberOTP DriverDetailsScreenState
                    | VerifyAlternateNumberOTP DriverDetailsScreenState
                    | RemoveAlternateNumber DriverDetailsScreenState
                    | GoToHomeScreen DriverDetailsScreenState
                    | UpdateGender DriverDetailsScreenState

data Action = NoAction
              | BackPressed
              | CallBackImageUpload String String String
              | RenderBase64Image
              | AfterRender
              | UploadFileAction
              | ClickAddAlternateButton
              | InAppKeyboardModalMobile InAppKeyboardModal.Action
              | InAppKeyboardModalOtp InAppKeyboardModal.Action
              | PopUpModalAction PopUpModal.Action
              | GenderSelectionModalAction SelectListModal.Action
              | ClickRemoveAlternateNumber
              | ClickEditAlternateNumber
              | PopUpModalActions PopUpModal.Action
              | GenderSelectionOpen


eval :: Action -> DriverDetailsScreenState -> Eval Action ScreenOutput DriverDetailsScreenState

eval BackPressed state = do
  if state.props.keyboardModalType /= NONE then continueWithCmd state[
      do 
        pure $ InAppKeyboardModalMobile (InAppKeyboardModal.BackPressed)
    ] 
  else if state.props.genderSelectionModalShow then continueWithCmd state[
      do
        pure $ GenderSelectionModalAction (SelectListModal.OnGoBack)
    ]
  else exit (GoBack state {data = state.data {driverEditAlternateMobile = Nothing} ,props = state.props { keyboardModalType = NONE,
    otpAttemptsExceeded = false,
    enterOtpFocusIndex = 0,
    otpIncorrect = false,
    alternateMobileOtp = "",
    removeNumberPopup = false,
    isEditAlternateMobile = false,
    numberExistError = false,
    genderSelectionModalShow = false}})

eval (CallBackImageUpload image imageName imagePath) state = if (image /= "") then
                                            continueWithCmd (state { data { base64Image = image}}) [do pure RenderBase64Image]
                                            else
                                              continue state

eval RenderBase64Image state = continueWithCmd state [do
  _ <- liftEffect $ renderBase64Image state.data.base64Image (getNewIDWithTag "EditProfileImage") true "CENTER_CROP"
  pure NoAction]
eval AfterRender state = continue state

eval UploadFileAction state = continue state

eval NoAction state = continue state

eval ClickAddAlternateButton state = do
  _ <- pure $ spy "Alternate Number" (getValueToLocalStore SET_ALTERNATE_TIME)
  let curr_time = getCurrentUTC ""
  let last_attempt_time = getValueToLocalStore SET_ALTERNATE_TIME
  let time_diff = runFn2 differenceBetweenTwoUTC curr_time last_attempt_time
  if(time_diff <= 600) then do
    pure $ toast (getString TOO_MANY_ATTEMPTS_PLEASE_TRY_AGAIN_LATER)
    continue state
  else
    continue state {props = state.props{keyboardModalType = MOBILE__NUMBER , isEditAlternateMobile = false}}

eval ClickEditAlternateNumber state = do
  _ <- pure $ spy " Edit Alternate Number" ""
  let curr_time = getCurrentUTC ""
  let last_attempt_time = getValueToLocalStore SET_ALTERNATE_TIME
  let time_diff = runFn2 differenceBetweenTwoUTC curr_time last_attempt_time
  if(time_diff <= 600) then do
   pure $ toast (getString TOO_MANY_ATTEMPTS_PLEASE_TRY_AGAIN_LATER)
   continue state
  else
    continue state {data = state.data {driverEditAlternateMobile = state.data.driverAlternateMobile}, props = state.props{keyboardModalType = MOBILE__NUMBER , isEditAlternateMobile = true, checkAlternateNumber = true}}

eval ClickRemoveAlternateNumber state = do
    continue state { props = state.props {removeNumberPopup = true}}

eval (InAppKeyboardModalMobile (InAppKeyboardModal.BackPressed)) state = do
  continue state { data = state.data {driverAlternateMobile = (if not state.props.isEditAlternateMobile then (Nothing) else state.data.driverAlternateMobile), driverEditAlternateMobile = Nothing}, props = state.props {keyboardModalType = NONE, checkAlternateNumber = not state.props.isEditAlternateMobile,numberExistError= false ,isEditAlternateMobile = false}}

eval (InAppKeyboardModalMobile (InAppKeyboardModal.OnClickTextCross)) state = do
  continue state {data {driverEditAlternateMobile = Nothing, driverAlternateMobile = (if not state.props.isEditAlternateMobile then (Nothing) else state.data.driverAlternateMobile)},props {numberExistError= false, checkAlternateNumber = true}}

eval (InAppKeyboardModalMobile (InAppKeyboardModal.OnSelection key index)) state = do
  let newVal = if not state.props.isEditAlternateMobile then ((fromMaybe "" state.data.driverAlternateMobile) <> key) else ((fromMaybe "" state.data.driverEditAlternateMobile) <> key)
  if(length newVal == 0) then
  continue state {data = state.data {driverAlternateMobile = (if (state.props.isEditAlternateMobile) then (state.data.driverAlternateMobile) else Nothing), driverEditAlternateMobile = Nothing }, props = state.props {checkAlternateNumber = not state.props.isEditAlternateMobile, numberExistError = false}}
  else if length newVal <= 10 then (do
              let isValidMobileNumber = case (charAt 0 newVal) of
                                    Just a -> if a=='0' || a=='1' || a=='2' then false
                                              else if a=='3' || a=='4' || a=='5' then(
                                                   if newVal=="4000400040" || newVal=="3000300030" || newVal=="5000500050" then
                                                   true
                                                   else false )
                                              else true
                                    Nothing -> true
              continue state { data = state.data { driverAlternateMobile = (if not state.props.isEditAlternateMobile then Just newVal else state.data.driverAlternateMobile), driverEditAlternateMobile = Just newVal }, props = state.props {checkAlternateNumber = isValidMobileNumber} }
  )else continue state

eval (InAppKeyboardModalMobile (InAppKeyboardModal.OnClickBack text)) state = do
  if(text == (getString ENTER_MOBILE_NUMBER))
    then continue state
    else do
    let newVal = (if length( text ) > 0 then (take (length ( text ) - 1 ) text) else "" )
    continue state { data = state.data {driverAlternateMobile = (if (not state.props.isEditAlternateMobile) then (if newVal=="" then Nothing else Just newVal) else state.data.driverAlternateMobile), driverEditAlternateMobile = (if newVal=="" then Nothing else Just newVal)  } , props = state.props { checkAlternateNumber = true, numberExistError = false}}

eval (InAppKeyboardModalMobile (InAppKeyboardModal.OnClickDone text)) state = do
  _ <- pure $ spy "Enter done" ""
  exit (ValidateAlternateNumber state {data = state.data {  driverAlternateMobile = (if (not state.props.isEditAlternateMobile) then Just text else state.data.driverAlternateMobile) , driverEditAlternateMobile = Just text}, props = state.props {keyboardModalType = OTP, numberExistError = false,otpIncorrect = false}})

eval (InAppKeyboardModalOtp (InAppKeyboardModal.OnClickResendOtp)) state = do
  exit (ResendAlternateNumberOTP state)


eval (InAppKeyboardModalOtp (InAppKeyboardModal.BackPressed)) state = do
  continue state { data = state.data {otpBackAlternateNumber = state.data.driverEditAlternateMobile } ,props = state.props {keyboardModalType = MOBILE__NUMBER, alternateMobileOtp = "", enterOtpFocusIndex = 0, otpIncorrect = false, otpAttemptsExceeded = false}}

eval (InAppKeyboardModalOtp (InAppKeyboardModal.OnClickBack text)) state = do
  let newVal = (if length( text ) > 0 then (take (length ( text ) - 1 ) text) else "" )
      focusIndex = length newVal
  continue state {props = state.props { alternateMobileOtp = newVal, enterOtpFocusIndex = focusIndex,otpIncorrect = false, otpAttemptsExceeded = false}}

eval (InAppKeyboardModalOtp (InAppKeyboardModal.OnSelection key index)) state = do
  let
    alternateMobileOtp = if (index + 1) > (length state.props.alternateMobileOtp) then ( take 4 (state.props.alternateMobileOtp <> key)) else (take index (state.props.alternateMobileOtp)) <> key <> (take 4 (drop (index+1) state.props.alternateMobileOtp))
    focusIndex = length alternateMobileOtp
  continue state { props = state.props { alternateMobileOtp = alternateMobileOtp, enterOtpFocusIndex = focusIndex, otpIncorrect = false } }

eval (PopUpModalAction (PopUpModal.OnButton1Click))
  state = continue state {props{ removeNumberPopup = false}}
eval (PopUpModalAction (PopUpModal.OnButton2Click)) state = do
  exit (RemoveAlternateNumber state {data = state.data {  driverAlternateMobile =Nothing }, props = state.props {removeNumberPopup = false, otpIncorrect = false, keyboardModalType = NONE,isEditAlternateMobile = false}})

eval (PopUpModalActions (PopUpModal.OnButton2Click)) state = do
   exit (GoToHomeScreen state {data = state.data{ driverAlternateMobile = (if(state.props.isEditAlternateMobile) then state.data.driverAlternateMobile else Nothing), driverEditAlternateMobile = Nothing } ,props =state.props{  otpIncorrect = false ,otpAttemptsExceeded = false ,keyboardModalType = NONE , alternateMobileOtp = "",checkAlternateNumber =(not state.props.isEditAlternateMobile) }})

eval (InAppKeyboardModalOtp (InAppKeyboardModal.OnClickDone text)) state = do
    exit (VerifyAlternateNumberOTP state)


eval (GenderSelectionModalAction (SelectListModal.UpdateIndex indexValue)) state = continue state { data = state.data { genderSelectionModal  { activeIndex = Just indexValue}}}
eval (GenderSelectionModalAction (SelectListModal.OnGoBack)) state = continue state { data { genderSelectionModal {activeIndex = if (state.data.driverGender == Nothing) then Nothing else getGenderIndex (fromMaybe "UNKNOWN" state.data.driverGender) (genders FunctionCall)}} ,props{ genderSelectionModalShow = false}}

eval GenderSelectionOpen state = do
  continue state{props {genderSelectionModalShow = true},data{genderSelectionModal{selectionOptions = genders FunctionCall}}}

eval (GenderSelectionModalAction (SelectListModal.Button2 PrimaryButtonController.OnClick)) state = do
    let genderSelected = case state.data.genderSelectionModal.activeIndex of
                                  Just index -> (state.data.genderSelectionModal.selectionOptions Array.!! (index))
                                  Nothing    -> Nothing
    _ <- pure $ spy "Gender Selected " genderSelected
    exit (UpdateGender state {data = state.data{driverGender = Just (fromMaybe dummyOptions genderSelected).reasonCode},props = state.props{genderSelectionModalShow = false}})

eval _ state = continue state

genders :: LazyCheck -> Array OptionButtonList
genders dummy =
  [ { reasonCode: "MALE"
    , description: (getString MALE)
    , textBoxRequired : false
    , subtext : Nothing
    }
  , { reasonCode: "FEMALE"
    , description: (getString FEMALE)
    , textBoxRequired : false
    , subtext : Nothing
    }
  , { reasonCode: "OTHER"
    , description: (getString OTHER)
    , textBoxRequired : false
    , subtext : Nothing
    }
  , { reasonCode: "PREFER_NOT_TO_SAY"
    , description: (getString PREFER_NOT_TO_SAY)
    , textBoxRequired : false
    , subtext : Nothing
    }
  ]

getTitle :: ListOptions -> String
getTitle listOptions =
  case listOptions of
    DRIVER_NAME_INFO -> (getString NAME)
    DRIVER_MOBILE_INFO -> (getString MOBILE_NUMBER)
    DRIVER_LICENCE_INFO -> (getString DRIVING_LICENSE)
    DRIVER_ALTERNATE_MOBILE_INFO -> (getString ALTERNATE_MOBILE_NUMBER)
    GENDER_INFO -> (getString GENDER)

getValue :: ListOptions -> DriverDetailsScreenState -> String
getValue listOptions state =
  case listOptions of
    DRIVER_NAME_INFO -> state.data.driverName
    DRIVER_MOBILE_INFO -> (fromMaybe "" (state.data.driverMobile))
    DRIVER_LICENCE_INFO -> state.data.drivingLicenseNo
    DRIVER_ALTERNATE_MOBILE_INFO -> (fromMaybe "" state.data.driverAlternateMobile)
    GENDER_INFO -> (fromMaybe (getString SET_NOW) (getGenderValue state.data.driverGender))

dummyOptions :: OptionButtonList
dummyOptions = 
  { reasonCode: ""
    , description: ""
    , textBoxRequired : false
    , subtext : Nothing
  }

getGenderValue :: Maybe String -> Maybe String
getGenderValue gender = 
  case gender of
    Just value -> case value of
      "MALE" -> Just (getString MALE)
      "FEMALE" -> Just (getString FEMALE)
      "OTHER" -> Just (getString OTHER)
      "PREFER_NOT_TO_SAY" -> Just (getString PREFER_NOT_TO_SAY)
      _ -> Nothing
    Nothing -> Nothing

getGenderState :: Maybe String -> Maybe String
getGenderState gender = 
  case gender of 
    Just value -> case value of
      "UNKNOWN" -> Nothing
      _ -> Just value
    Nothing -> Nothing