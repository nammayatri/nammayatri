{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AddVehicleDetailsScreen.Controller where

import Prelude (Unit, bind, pure, ($), class Show, unit, (/=), discard, (==), (&&), (||), not, (<=),(>), (<>), (<), show, (+))
import Effect (Effect)
import PrestoDOM (Eval, Props, continue, continueWithCmd, exit, updateAndExit)
import Screens.Types (AddVehicleDetailsScreenState, VehicalTypes(..))
-- import Tracker (trackAction)
import PrestoDOM.Types.Core (class Loggable)
import Components.PrimarySelectItem.Controller as PrimarySelectItem
import Engineering.Helpers.Commons (getNewIDWithTag)
import JBridge (disableActionEditText, uploadFile, hideKeyboardOnNavigation, openWhatsAppSupport)
import Effect.Class (liftEffect)
import Components.SelectVehicleTypeModal.Controller as SelectVehicleTypeModal
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.RegistrationModal.Controller as RegistrationModalController
import Components.OnboardingHeader.Controller as OnboardingHeaderController
import Components.TutorialModal.Controller as TutorialModalController
import Components.ReferralMobileNumber.Controller as ReferralMobileNumberController
import Components.GenericMessageModal.Controller as GenericMessageModalController
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Log (printLog, trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Data.String.CodeUnits (charAt)
import Data.String (length)
import Screens (ScreenName(..), getScreen)
import Data.Maybe
import Debug.Trace (spy)
import Data.String(length)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of 
    AfterRender -> trackAppScreenRender appId "screen" (getScreen ADD_VEHICLE_DETAILS_SCREEN)
    BackPressed flag -> do
      trackAppBackPress appId (getScreen ADD_VEHICLE_DETAILS_SCREEN)
      if flag then trackAppScreenEvent appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "backpress_in_open_rc_manual"
        else trackAppEndScreen appId (getScreen ADD_VEHICLE_DETAILS_SCREEN)
    PrimarySelectItemAction (PrimarySelectItem.OnClick item) -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "primary_select_item" "vehicle_type_on_click"
    VehicleRegistrationNumber str -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "vehicle_registration_number_on_click"
    UploadFile -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "upload_file_on_click"
    CallBackImageUpload str imageName -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "callback_image_upload"
    SelectVehicleTypeModalAction act -> case act of
      SelectVehicleTypeModal.OnCloseClick -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "select_vehicle_type_modal" "on_close_click"
      SelectVehicleTypeModal.OnSelect item -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "select_vehicle_type_modal" "vehicle_type_on_select"
    VehicleModelName str -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "vehicle_modal_name_on_click"
    VehicleColour str -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "vehicle_colour_on_click"
    RemoveUploadedFile -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "remove_uploaded_file_on_click"
    ScreenClick -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "screen_click"
    OnboardingHeaderAction act -> case act of
      OnboardingHeaderController.TriggerRegModal -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "onboarding_header" "trigger_registration_modal"
      OnboardingHeaderController.BackPressed -> do
        trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "onboarding_header" "backpressed"
        trackAppEndScreen appId (getScreen ADD_VEHICLE_DETAILS_SCREEN)
    RegistrationModalAction (RegistrationModalController.OnCloseClick) -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "registration_modal" "on_close_click"
    PrimaryButtonAction (PrimaryButtonController.OnClick) -> do
      trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "primary_button" "on_click"
      trackAppEndScreen appId (getScreen ADD_VEHICLE_DETAILS_SCREEN)
    PrimaryButtonAction (PrimaryButtonController.NoAction) -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "primary_button" "no_action"
    TutorialModalAction act -> case act of
      TutorialModalController.OnCloseClick -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "tutorial_modal" "on_close_click"
      TutorialModalController.CallSupport -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "call_support_onclick"
      TutorialModalController.Logout -> do
        trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "Logout"
        trackAppEndScreen appId (getScreen ADD_VEHICLE_DETAILS_SCREEN)
    TutorialModal manual -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "trigger_rc_manual"
    VehicleRCNumber str -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "vehicle_rc_number"
    ReferralMobileNumberAction act -> case act of
      ReferralMobileNumberController.OnBackClick -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "referral_mobile_number" "on_back_icon_click"
      ReferralMobileNumberController.PrimaryButtonActionController act -> case act of
        PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "referral_mobile_number" "primary_button_on_click"
        PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "referral_mobile_number" "primary_button_no_action"
      ReferralMobileNumberController.PrimaryEditTextActionController (PrimaryEditTextController.TextChanged valId newVal) -> trackAppTextInput appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "referral_mobile_number_text_changed" "primary_edit_text"
    ReferralMobileNumber -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "trigger_referral_mobile_number"
    GenericMessageModalAction act -> case act of
      GenericMessageModalController.PrimaryButtonActionController act -> case act of
        PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "generic_message_modal" "primary_button_next_on_click"
        PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "generic_message_modal" "primary_button_no_action"
    ReEnterVehicleRegistrationNumber val -> trackAppTextInput appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "vehicle_registration_number_text_changed" "in_screen"
    WhatsAppSupport -> trackAppScreenEvent appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "whatsAppSupport"
    PreviewImageAction -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "preview_image"
    DatePickerAction -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "date_picker"
    DatePicker year month date -> trackAppScreenEvent appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "date_picker"
    NoAction -> trackAppScreenEvent appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "no_action"

data ScreenOutput = GoToApplicationSubmitted AddVehicleDetailsScreenState 
                    | GoBack AddVehicleDetailsScreenState
                    | ValidateImageAPICall AddVehicleDetailsScreenState 
                    | ReferApiCall AddVehicleDetailsScreenState
                    | ApplicationSubmittedScreen
                    | LogoutAccount

data Action =   WhatsAppSupport | BackPressed Boolean | PrimarySelectItemAction PrimarySelectItem.Action | NoAction
  | VehicleRegistrationNumber String
  | ReEnterVehicleRegistrationNumber String
  | UploadFile
  | CallBackImageUpload String String
  | SelectVehicleTypeModalAction SelectVehicleTypeModal.Action
  | VehicleModelName String
  | VehicleColour String
  | RemoveUploadedFile
  | ScreenClick
  | OnboardingHeaderAction OnboardingHeaderController.Action
  | RegistrationModalAction RegistrationModalController.Action
  | PrimaryButtonAction PrimaryButtonController.Action
  | TutorialModalAction TutorialModalController.Action
  | TutorialModal String 
  | VehicleRCNumber String
  | AfterRender
  | ReferralMobileNumberAction ReferralMobileNumberController.Action
  | GenericMessageModalAction GenericMessageModalController.Action
  | ReferralMobileNumber
  | DatePicker Int Int Int
  | PreviewImageAction
  | DatePickerAction


eval :: Action -> AddVehicleDetailsScreenState -> Eval Action ScreenOutput AddVehicleDetailsScreenState
eval AfterRender state = continue state
eval (BackPressed flag) state = do
            if(state.props.openRCManual) then continue state{props{openRCManual = false}} 
              else exit $ GoBack state
eval (OnboardingHeaderAction (OnboardingHeaderController.TriggerRegModal)) state = continue state { props = state.props { openRegistrationModal = true } }
eval (OnboardingHeaderAction (OnboardingHeaderController.BackPressed)) state = exit $ GoBack state
eval (RegistrationModalAction (RegistrationModalController.OnCloseClick)) state = do 
  continue state { props = state.props { openRegistrationModal = false } }
eval (PrimarySelectItemAction (PrimarySelectItem.OnClick item)) state = do 
  _ <- pure $ hideKeyboardOnNavigation true
  continue $ (state {props = state.props {
    openSelectVehicleTypeModal = true
    }
  })
eval ScreenClick state = do 
  let newState = state { props = state.props { openSelectVehicleTypeModal = false }}
  continue newState
eval RemoveUploadedFile state = do 
  let newState = state { props = state.props { rcAvailable = false, rc_name = "", isValidState = false }, data = state.data { rc_base64 = "" }}
  continue newState
eval (VehicleRegistrationNumber val) state = do 
  let newState = state {data = state.data { vehicle_registration_number = val }, props = state.props{isValidState = (checkRegNum (val) && state.props.rcAvailable) }}
  continue newState
eval (ReEnterVehicleRegistrationNumber val) state = do 
  let newState = state {data = state.data { reEnterVehicleRegistrationNumber = val }, props = state.props{isValidState = (checkRegNum (val) && state.props.rcAvailable) }}
  continue newState
eval (VehicleModelName val) state = do 
  _ <- pure $ disableActionEditText (getNewIDWithTag "VehicleModelName")
  let newState = state {data = state.data { vehicle_model_name = val }}
  continue newState
eval (VehicleColour val) state = do 
  _ <- pure $ disableActionEditText (getNewIDWithTag "VehicleColour")
  let newState = state {data = state.data { vehicle_color = val }}
  continue newState
eval (CallBackImageUpload base_64 imageName) state = do
  _ <- pure $ printLog "base_64 CallBackImageUpload" base_64
  _ <- pure $ printLog "imageName" imageName
  if base_64 /= "" then do 
    let newState = state { props = state.props { rcAvailable = true, rc_name = imageName, isValidState = (checkRegNum (state.data.vehicle_registration_number))}, data = state.data { rc_base64 = base_64 }}
    exit $ ValidateImageAPICall newState
    else continue state{props{isValidState = false}}
eval (UploadFile) state = continueWithCmd state [do
        _ <- liftEffect $ uploadFile unit 
        pure NoAction]
eval (VehicleRCNumber val) state = do 
  _ <- pure $ disableActionEditText (getNewIDWithTag "VehicleRCNumber")
  let newState = state {data = state.data { vehicle_rc_number = val }}
  continue newState

eval (SelectVehicleTypeModalAction (SelectVehicleTypeModal.OnCloseClick)) state = do 
  _ <- pure $ hideKeyboardOnNavigation true
  continue $ (state {props = state.props {
    openSelectVehicleTypeModal = false
  }
})
eval (SelectVehicleTypeModalAction (SelectVehicleTypeModal.OnSelect item)) state = let
    newState = 
      state {
      props = state.props {
        openSelectVehicleTypeModal = false
      }, data = state.data {
        vehicle_type = (case item of
                        Sedan     -> "Sedan"
                        SUV       -> "SUV"
                        Hatchback -> "Hatchback"
                        Auto      -> "Auto")
      }
    }
   
    in continueWithCmd newState [ do
            --_ <- trackAction Tracker.User Tracker.Info ON_CLICK  "vehicle_type_select_click" (unsafeToForeign item) Object.empty
            pure NoAction
    ]
eval (TutorialModal manual) state = do
  pure $ hideKeyboardOnNavigation true 
  case manual of 
    "REGISTERATION_DATE" -> continue state{props{openRegistrationDateManual = true}}
    "RC" -> continue state{props{openRCManual = true}}
    _ -> continue state
eval (TutorialModalAction (TutorialModalController.OnCloseClick)) state = continue state{props{openRCManual = false, openRegistrationDateManual = false}}
eval (TutorialModalAction (TutorialModalController.CallSupport)) state = continueWithCmd state [do
  _ <- liftEffect $ openWhatsAppSupport "+918618963188"
  pure WhatsAppSupport
  ]
eval (TutorialModalAction (TutorialModalController.Logout)) state = exit LogoutAccount
eval ReferralMobileNumber state = do
  continue state{props{openReferralMobileNumber = true, btnActive = false, isEdit = true}}
eval (ReferralMobileNumberAction (ReferralMobileNumberController.OnBackClick)) state = continue state{props{openReferralMobileNumber = false}}
eval (ReferralMobileNumberAction (ReferralMobileNumberController.PrimaryButtonActionController (PrimaryButtonController.OnClick))) state = exit $ ReferApiCall $ state { props { openReferralMobileNumber = false, referralViewstatus = true }}
eval (ReferralMobileNumberAction (ReferralMobileNumberController.PrimaryEditTextActionController (PrimaryEditTextController.TextChanged valId newVal))) state = do
  let var =  if( (charAt 0 newVal) == Just '0' || (charAt 0 newVal) == Just '1') then true else false
  _ <- pure $ spy "new val" state 
  _ <- if length newVal == 10 then do
            pure $ hideKeyboardOnNavigation true
            else pure unit
  continue state { props = state.props { btnActive = if (length newVal == 10 &&  not var) then true else false
                                        , isValid = var
                                        , isEdit = if (length newVal == 10 && state.props.isEdit) then true else false }
                                        , data = state.data { referral_mobile_number = if length newVal <= 10 then newVal else state.data.referral_mobile_number}}
eval (PrimaryButtonAction (PrimaryButtonController.OnClick)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  updateAndExit state $ (GoToApplicationSubmitted state)
eval (GenericMessageModalAction (GenericMessageModalController.PrimaryButtonActionController (PrimaryButtonController.OnClick))) state = exit ApplicationSubmittedScreen

eval (DatePicker year month date) state = do
  continue state {data = state.data { dateOfRegistration = Just $ (dateFormat year) <> "-" <> (dateFormat (month+1)) <> "-" <> (dateFormat date) <> " 00:00:00.233691+00" , dateOfRegistrationView = (show date) <> "/" <> (show (month+1)) <> "/" <> (show year), rcImageID = "null"}} -- rcImageID made null to handle fallback

eval DatePickerAction state = continue state

eval PreviewImageAction state = continue state

eval _ state = continue state

checkRegNum :: String -> Boolean
checkRegNum temp = if (length temp > 1) then true else false
  
overrides :: String -> (Action -> Effect Unit) -> AddVehicleDetailsScreenState -> Props (Effect Unit)
overrides _ push state = []

dateFormat :: Int -> String
dateFormat date = if date < 10 then "0" <> (show date) else (show date)
