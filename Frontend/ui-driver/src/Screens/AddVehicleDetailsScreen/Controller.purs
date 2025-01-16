{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AddVehicleDetailsScreen.Controller where

import Data.Maybe

import Common.Types.App (LazyCheck(..), UploadFileConfig(..))
import Components.GenericMessageModal.Controller as GenericMessageModalController
import Components.OnboardingHeader.Controller as OnboardingHeaderController
import Components.PopUpModal.Controller as PopUpModal
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.PrimarySelectItem.Controller as PrimarySelectItem
import Components.ReferralMobileNumber.Controller as ReferralMobileNumberController
import Components.RegistrationModal.Controller as RegistrationModalController
import Components.SelectVehicleTypeModal.Controller as SelectVehicleTypeModal
import Components.GenericHeader as GenericHeader
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import Components.TutorialModal.Controller as TutorialModalController
import Components.ValidateDocumentModal.Controller as ValidateDocumentModal
import Data.Array (elem)
import Data.String (length, split, Pattern(..), toUpper)
import Data.String.CodeUnits (charAt)
import Debug (spy)
import Effect (Effect)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Helpers.Utils (contactSupportNumber)
import JBridge (disableActionEditText, hideKeyboardOnNavigation, openWhatsAppSupport, renderCameraProfilePicture, showDialer, uploadFile, renderBase64ImageFile)
import Log (printLog, trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude (Unit, bind, pure, ($), class Show, unit, (/=), discard, (==), (&&), (||), not, (<=), (>), (<>), (<), show, (+), void)
import PrestoDOM (Eval, update, Props, continue, continueWithCmd, exit, updateAndExit, toast)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (AddVehicleDetailsScreenState, VehicalTypes(..), StageStatus(..))
import Services.Config (getSupportNumber, getWhatsAppSupportNo)
import Effect.Unsafe (unsafePerformEffect)
import ConfigProvider
import Effect.Uncurried (runEffectFn4)
import Components.OptionsMenu as OptionsMenu
import Components.BottomDrawerList as BottomDrawerList
import Screens.Types as ST
import Storage (KeyStore(..), getValueToLocalStore)
import JBridge as JB
import Components.RequestInfoCard as RequestInfoCard
import Engineering.Helpers.Events as EHE

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
    CallBackImageUpload str imageName imagePath -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "callback_image_upload"
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
      ReferralMobileNumberController.PrimaryEditTextActionController act -> case act of 
        PrimaryEditTextController.TextChanged valId newVal -> trackAppTextInput appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "referral_mobile_number_text_changed" "primary_edit_text"
        PrimaryEditTextController.FocusChanged _ -> trackAppTextInput appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "referral_mobile_number_text_focus_changed" "primary_edit_text"
        PrimaryEditTextController.TextImageClicked -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "referral_mobile_number" "text_image_onclick"
      ReferralMobileNumberController.OnSubTextClick -> pure unit
    ReferralMobileNumber -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "trigger_referral_mobile_number"
    GenericMessageModalAction act -> case act of
      GenericMessageModalController.PrimaryButtonActionController act -> case act of
        PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "generic_message_modal" "primary_button_next_on_click"
        PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "generic_message_modal" "primary_button_no_action"
    ReEnterVehicleRegistrationNumber val -> trackAppTextInput appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "vehicle_registration_number_text_changed" "in_screen"
    WhatsAppSupport -> trackAppScreenEvent appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "whatsAppSupport"
    PreviewImageAction -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "preview_image"
    DatePickerAction -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "date_picker"
    DatePicker resp year month date -> trackAppScreenEvent appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "date_picker"
    NoAction -> trackAppScreenEvent appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "no_action"
    PopUpModalLogoutAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "popup_modal" "on_goback"
      PopUpModal.DismissPopup -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "popup_modal" "dismiss_popup"
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "popup_modal" "call_support"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "popup_modal_action" "no_action"
      PopUpModal.OnSecondaryTextClick -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "popup_modal_action" "secondary_text"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "popup_modal_action" "image"
      PopUpModal.OptionWithHtmlClick -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "popup_modal_action" "options_with_html"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "popup_modal_action" "primary_edit_text"
      PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "popup_modal_action" "countdown_updated"
      PopUpModal.YoutubeVideoStatus _ -> trackAppScreenEvent appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "popup_modal_action" "youtube_video_status"
      _ -> pure unit
    ValidateDocumentModalAction act -> case act of
      ValidateDocumentModal.BackPressed  -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "validate_document_modal" "backpressed"
      ValidateDocumentModal.AfterRender ->  trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "validate_document_modal" "afterrender"
      _ -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "validate_document_modal" "no_action"
    PopUpModalActions act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "popup_modal" "on_goback"
      PopUpModal.DismissPopup -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "popup_modal" "dismiss_popup"
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "popup_modal" "call_support"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "popup_modal_action" "no_action"
      PopUpModal.OnSecondaryTextClick -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "popup_modal_action" "secondary_text"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "popup_modal_action" "image"
      PopUpModal.OptionWithHtmlClick -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "popup_modal_action" "options_with_html"      
      PopUpModal.YoutubeVideoStatus _ -> trackAppScreenEvent appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "popup_modal_action" "youtube_video_status"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "popup_modal_action" "primary_edit_text"
      PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "popup_modal_action" "countdown_updated"
      _ -> pure unit
    RenderProfileImage image id -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "renderImage" "afterrender"
    RedirectScreen -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "redirect_screem" "no_action"
    ChangeLocation -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "change_location" "on_click"
    ActivateRCbtn act -> case act of
      PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "activate_rc_btn" "on_click"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "activate_rc_btn" "no_action"
    CancelButtonMultiRCPopup -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "cancel_button_multi_rc_popup" "on_click"
    AppOnboardingNavBarAC act -> case act of
      AppOnboardingNavBar.GenericHeaderAC genericHeaderAction -> case genericHeaderAction of 
        GenericHeader.PrefixImgOnClick -> trackAppScreenEvent appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "generic_header_on_click"
        GenericHeader.SuffixImgOnClick -> trackAppScreenEvent appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "generic_header_on_click"
      AppOnboardingNavBar.Logout -> trackAppScreenEvent appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "onboarding_nav_bar_logout"
      AppOnboardingNavBar.PrefixImgOnClick -> trackAppScreenEvent appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "app_onboarding_nav_bar_prefix_img_on_click"
    SkipButton -> trackAppActionClick appId (getScreen ADD_VEHICLE_DETAILS_SCREEN) "in_screen" "skip_button_click"
    ListExpandAinmationEnd -> trackAppScreenEvent appId (getScreen TRIP_DETAILS_SCREEN) "in_screen" "list_expand_animation_end"
    _ -> pure unit

data ScreenOutput = ValidateDetails AddVehicleDetailsScreenState
                    | GoBack AddVehicleDetailsScreenState
                    | ValidateDataAPICall AddVehicleDetailsScreenState
                    | ReferApiCall AddVehicleDetailsScreenState
                    | ApplicationSubmittedScreen
                    | LogoutAccount
                    | GoToRegisteration 
                    | GoToDriverProfile
                    | ActivateRC AddVehicleDetailsScreenState
                    | ChangeVehicle AddVehicleDetailsScreenState
                    | SelectLang AddVehicleDetailsScreenState
                    
                    


data Action =   WhatsAppSupport | BackPressed Boolean | PrimarySelectItemAction PrimarySelectItem.Action | NoAction
  | VehicleRegistrationNumber String
  | ReEnterVehicleRegistrationNumber String
  | UploadFile
  | CallBackImageUpload String String String
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
  | DatePicker String Int Int Int
  | PreviewImageAction
  | DatePickerAction
  | PopUpModalLogoutAction PopUpModal.Action
  | ValidateDocumentModalAction ValidateDocumentModal.Action
  | RenderProfileImage String String
  | PopUpModalActions PopUpModal.Action
  | RedirectScreen
  | ChangeLocation
  | ActivateRCbtn PrimaryButtonController.Action
  | CancelButtonMultiRCPopup
  | AppOnboardingNavBarAC AppOnboardingNavBar.Action
  | SkipButton
  | OptionsMenuAction OptionsMenu.Action
  | ChangeVehicleAC PopUpModal.Action
  | BottomDrawerListAC BottomDrawerList.Action
  | WhatsAppClick
  | SelectButton Int
  | OpenAcModal
  | RequestInfoCardAction RequestInfoCard.Action
  | SelectAmbulanceFacility
  | ListExpandAinmationEnd
  | SelectAmbulanceVarient String
  | OpenAmbulanceFacilityModal
  | RequestAmbulanceFacility RequestInfoCard.Action
  | AgreePopUp PopUpModal.Action
  | ButtonClick

uploadFileConfig :: UploadFileConfig
uploadFileConfig = UploadFileConfig {
  showAccordingToAspectRatio : false,
  imageAspectHeight : 0,
  imageAspectWidth : 0
}

eval :: Action -> AddVehicleDetailsScreenState -> Eval Action ScreenOutput AddVehicleDetailsScreenState
eval AfterRender state = 
                 if (state.props.validateProfilePicturePopUp == true) then do 
                   continueWithCmd state [do pure (RenderProfileImage state.data.rc_base64 (getNewIDWithTag "ValidateProfileImage"))]  
                 else continue state

eval (RenderProfileImage image id) state = do
  continueWithCmd state [do 
    void $ liftEffect $ runEffectFn4 renderBase64ImageFile image id true "CENTER_CROP"
    pure NoAction]

eval (BackPressed flag) state = do
    _ <- pure $ hideKeyboardOnNavigation true
    if(state.props.validateProfilePicturePopUp) then do
        if (state.props.fileCameraOption) then do
            continueWithCmd (state {props{ validateProfilePicturePopUp = false,imageCaptureLayoutView = true}}) [ pure UploadFile]
        else do
            continueWithCmd state {props { validateProfilePicturePopUp = false, fileCameraPopupModal = false, fileCameraOption = false, imageCaptureLayoutView = false}} [do
                _ <- liftEffect $ uploadFile uploadFileConfig true
                pure NoAction]
    else if(state.props.imageCaptureLayoutView) then continue state{props{imageCaptureLayoutView = false,openHowToUploadManual = true}} 
    else if(state.props.fileCameraPopupModal) then continue state{props{fileCameraPopupModal = false, validateProfilePicturePopUp = false, imageCaptureLayoutView = false}} 
    else if(state.props.openHowToUploadManual) then continue state{props{openHowToUploadManual = false}} 
    else if(state.props.openRCManual) then continue state{props{openRCManual = false}}
    else if state.props.confirmChangeVehicle then continue state{props{confirmChangeVehicle = false}}
    else if state.props.menuOptions then continue state{props{menuOptions = false}} 
    else if state.props.contactSupportModal == ST.SHOW then continue state { props { contactSupportModal = ST.ANIMATING}}
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
  let newState = state {data = state.data { vehicle_registration_number = toUpper val }, props = state.props{isValidState = (checkRegNum (toUpper val) && state.props.rcAvailable) }}
  if (length val == 10) then void $ pure $ EHE.addEvent (EHE.defaultEventObject "rc_number_entered") { module = "vehicle_registration_page", source = "RC"} 
  else pure unit
  continue newState
eval (ReEnterVehicleRegistrationNumber val) state = do
  let newState = state {data = state.data { reEnterVehicleRegistrationNumber = toUpper val }, props = state.props{isValidState = (checkRegNum (toUpper val) && state.props.rcAvailable) }}
  if (length val == 10) then void $ pure $ EHE.addEvent (EHE.defaultEventObject "rc_number_confirmed") { module = "vehicle_registration_page", source = "RC"}
  else pure unit
  continue newState
eval (VehicleModelName val) state = do
  _ <- pure $ disableActionEditText (getNewIDWithTag "VehicleModelName")
  let newState = state {data = state.data { vehicle_model_name = val }}
  continue newState
eval (VehicleColour val) state = do
  _ <- pure $ disableActionEditText (getNewIDWithTag "VehicleColour")
  let newState = state {data = state.data { vehicle_color = val }}
  continue newState
eval (CallBackImageUpload base_64 imageName imagePath) state = do
  _ <- pure $ printLog "base_64 CallBackImageUpload" base_64
  _ <- pure $ printLog "imageName" imageName
  if base_64 /= "" then do
    let newState = state { props = state.props { validateProfilePicturePopUp = true, imageCaptureLayoutView = false, rcAvailable = true, rc_name = imageName, isValidState = (checkRegNum (state.data.vehicle_registration_number))}, data = state.data { rc_base64 = base_64 }}
    continueWithCmd newState [ do
      void $ runEffectFn4 renderBase64ImageFile base_64 (getNewIDWithTag "ValidateProfileImage") false "CENTER_CROP"
      pure $ ValidateDocumentModalAction (ValidateDocumentModal.PrimaryButtonActionController (PrimaryButtonController.OnClick))]
    else continue state{props{isValidState = false}}
eval (UploadFile) state = continueWithCmd (state {props {validateProfilePicturePopUp = false, imageCaptureLayoutView = true}}) [do
     _ <- liftEffect $ renderCameraProfilePicture (getNewIDWithTag "ProfilePictureCaptureLayout")
     pure NoAction
      ]
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
                        Auto      -> "Auto"
                        Bike      -> "Bike"
                        Ambulance_Taxi -> "Ambulance_Taxi"
                        Ambulance_Taxi_Oxy -> "Ambulance_Taxi_Oxy"
                        Ambulance_AC -> "Ambulance_AC"
                        Ambulance_AC_Oxy -> "Ambulance_AC_Oxy"
                        Ambulance_Ventilator -> "Ambulance_Ventilator"
                        EV_Auto -> "EV Auto"
                        Suv_Plus  -> "SUV_PLUS"
                        HERITAGE_CAB -> "Heritage Cab")
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
  let merchant = getMerchant FunctionCall
  _ <- case merchant of
    NAMMAYATRI -> pure $ unsafePerformEffect $ contactSupportNumber "WHATSAPP"  -- unsafePerformEffect -> Temporary fix , need to update later
    YATRI -> pure $ unsafePerformEffect $ contactSupportNumber "WHATSAPP"
    YATRISATHI -> openWhatsAppSupport $ getWhatsAppSupportNo $ show merchant
    _ -> pure $ showDialer (getSupportNumber "") false
  pure NoAction
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
  let agreeTermsModalValue = state.data.vehicleCategory ==  Just (ST.AmbulanceCategory)
  if agreeTermsModalValue && not state.props.openHowToUploadManual then continue state { props { agreeTermsModal = agreeTermsModalValue }}
  else if isJust state.data.dateOfRegistration then exit $ ValidateDataAPICall state
  else if (not state.props.openHowToUploadManual) then do
    let _ = EHE.addEvent (EHE.defaultEventObject "upload_rc_clicked") { module = "vehicle_registration_page", source = "RC" }
    let _ = EHE.addEvent (EHE.defaultEventObject "upload_rc_page_loaded") { module = "vehicle_registration_page", source = "RC" }
    continue state {props {openHowToUploadManual = true}}
  else  continueWithCmd state {props { fileCameraPopupModal = false, fileCameraOption = false}} [do
    let _ = EHE.addEvent (EHE.defaultEventObject "take_rc_photo_clicked") { module = "vehicle_registration_page", source = "RC"}
    _ <- liftEffect $ uploadFile uploadFileConfig true
    pure NoAction]
eval (GenericMessageModalAction (GenericMessageModalController.PrimaryButtonActionController (PrimaryButtonController.OnClick))) state = exit ApplicationSubmittedScreen

eval ButtonClick state = do
  if isJust state.data.dateOfRegistration then exit $ ValidateDataAPICall state
  else if (not state.props.openHowToUploadManual) then 
    continue state {props {openHowToUploadManual = true}}
  else  continueWithCmd state {props { fileCameraPopupModal = false, fileCameraOption = false}} [do
     _ <- liftEffect $ uploadFile uploadFileConfig true
     pure NoAction]

eval SkipButton state = exit $ ValidateDataAPICall state

eval (DatePicker resp year month date) state = do
  case resp of 
    "SELECTED" -> continue state {data = state.data { dateOfRegistration = Just $ (dateFormat year) <> "-" <> (dateFormat (month+1)) <> "-" <> (dateFormat date) <> " 00:00:00.233691+00" , dateOfRegistrationView = (show date) <> "/" <> (show (month+1)) <> "/" <> (show year), rcImageID = "null"}
                                  , props {isDateClickable = true}} -- rcImageID made null to handle fallback
    _ -> continue state {props {isDateClickable = true}}

eval DatePickerAction state = continue state {props {isDateClickable = false}}

eval PreviewImageAction state = continue state

eval (PopUpModalLogoutAction (PopUpModal.OnButton2Click)) state = continue $ (state {props {logoutModalView= false}})

eval (PopUpModalLogoutAction (PopUpModal.OnButton1Click)) state = exit $ LogoutAccount

eval (AgreePopUp (PopUpModal.OnButton2Click)) state = continue $ (state {props {agreeTermsModal= false}})

eval (AgreePopUp (PopUpModal.OnButton1Click)) state = continueWithCmd state{props{ agreeTermsModal = false}} [pure $ ButtonClick]

eval (AppOnboardingNavBarAC (AppOnboardingNavBar.Logout)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  continue state {props{menuOptions = not state.props.menuOptions}}

eval (AppOnboardingNavBarAC AppOnboardingNavBar.PrefixImgOnClick) state = continueWithCmd state [ do pure $ BackPressed false]

eval (ValidateDocumentModalAction (ValidateDocumentModal.AfterRender)) state = do
 continueWithCmd state [do pure (AfterRender)] 

eval (ValidateDocumentModalAction (ValidateDocumentModal.BackPressed)) state = do
 continueWithCmd state [do pure (BackPressed false)]  

eval (ValidateDocumentModalAction (ValidateDocumentModal.PrimaryButtonActionController (PrimaryButtonController.OnClick))) state = do
   if (not state.props.errorVisibility) then do
    updateAndExit state{props{validating = true}} $ ValidateDetails state{props{validating = true}}
   else  
     continueWithCmd state {props {validateProfilePicturePopUp = false, errorVisibility = false, fileCameraPopupModal = false, fileCameraOption = false}, data{errorMessage = ""}} [do
     let _ = EHE.addEvent (EHE.defaultEventObject "retake_photo_clicked") { module = "vehicle_registration_page", source = "RC"}
     _ <- liftEffect $ uploadFile uploadFileConfig true
     pure NoAction]

eval (PopUpModalActions (PopUpModal.OnButton2Click)) state = do
   continueWithCmd state {props { fileCameraPopupModal = false, fileCameraOption = false}} [do
     _ <- liftEffect $ uploadFile uploadFileConfig true
     pure NoAction]

eval (PopUpModalActions (PopUpModal.OnButton1Click)) state = do
       continueWithCmd (state {props{ validateProfilePicturePopUp = false,imageCaptureLayoutView = true, fileCameraPopupModal = false, fileCameraOption = true, openHowToUploadManual = false}}) [ pure UploadFile]

eval RedirectScreen state = exit GoToRegisteration

eval ChangeLocation state = exit $ LogoutAccount

eval (ActivateRCbtn (PrimaryButtonController.OnClick)) state = case state.props.multipleRCstatus of
                                                                COMPLETED -> exit $ ActivateRC state
                                                                _ -> exit $ GoToDriverProfile

eval CancelButtonMultiRCPopup state = case state.props.multipleRCstatus of
                                        COMPLETED -> exit $ GoToDriverProfile
                                        _ -> continueWithCmd state [do
                                          let merchant = getMerchant FunctionCall
                                          _ <- case merchant of
                                            NAMMAYATRI -> contactSupportNumber "WHATSAPP" --getWhatsAppSupportNo $ show merchant
                                            YATRI -> contactSupportNumber "WHATSAPP"
                                            YATRISATHI -> openWhatsAppSupport $ getWhatsAppSupportNo $ show merchant
                                            _ -> pure $ showDialer (getSupportNumber "") false
                                          pure NoAction
                                          ]

eval (OptionsMenuAction OptionsMenu.BackgroundClick) state = continue state{props{menuOptions = false}}

eval (OptionsMenuAction (OptionsMenu.ItemClick item)) state = do
  let newState = state{props{menuOptions = false}}
  case item of
    "logout" -> continue newState {props { logoutModalView = true }}
    "contact_support" -> continue newState { props { contactSupportModal = ST.SHOW}}
    "change_vehicle" -> continue newState {props {confirmChangeVehicle = true}}
    "change_language" -> exit $ SelectLang newState
    _ -> continue newState

eval (ChangeVehicleAC (PopUpModal.OnButton2Click)) state = continue state {props {confirmChangeVehicle= false}}

eval (ChangeVehicleAC (PopUpModal.OnButton1Click)) state = exit $ ChangeVehicle state

eval (ChangeVehicleAC (PopUpModal.DismissPopup)) state = continue state {props {confirmChangeVehicle= false}}

eval (BottomDrawerListAC BottomDrawerList.Dismiss) state = continue state { props { contactSupportModal = ST.ANIMATING}}

eval (BottomDrawerListAC BottomDrawerList.OnAnimationEnd) state = continue state { props { contactSupportModal = if state.props.contactSupportModal == ST.ANIMATING then ST.HIDE else state.props.contactSupportModal}}

eval (BottomDrawerListAC (BottomDrawerList.OnItemClick item)) state = do
  case item.identifier of
    "whatsapp" -> continueWithCmd state [pure WhatsAppClick]
    "call" ->  continueWithCmd state [do
                let merchant = getMerchant FunctionCall
                _ <- case merchant of
                  NAMMAYATRI -> pure $ unsafePerformEffect $ contactSupportNumber "WHATSAPP"  -- unsafePerformEffect -> Temporary fix , need to update later
                  YATRI -> pure $ unsafePerformEffect $ contactSupportNumber "WHATSAPP"
                  YATRISATHI -> openWhatsAppSupport $ getWhatsAppSupportNo $ show merchant
                  _ -> pure $ showDialer (getSupportNumber "") false
                pure NoAction
  ]
    _ -> continue state

eval WhatsAppClick state = continueWithCmd state [do
  let supportPhone = state.data.cityConfig.registration.supportWAN
      phone = "%0APhone%20Number%3A%20"<> getValueToLocalStore MOBILE_NUMBER_KEY
      dlNumber = getValueToLocalStore ENTERED_DL
      rcNumber = getValueToLocalStore ENTERED_RC
      dl = if (dlNumber /= "__failed") then ("%0ADL%20Number%3A%20"<> dlNumber) else ""
      rc = if (rcNumber /= "__failed") then ("%0ARC%20Number%3A%20"<> rcNumber) else ""
  void $ JB.openUrlInApp $ "https://wa.me/" <> supportPhone <> "?text=Hi%20Team%2C%0AI%20would%20require%20help%20in%20onboarding%20%0A%E0%A4%AE%E0%A5%81%E0%A4%9D%E0%A5%87%20%E0%A4%AA%E0%A4%82%E0%A4%9C%E0%A5%80%E0%A4%95%E0%A4%B0%E0%A4%A3%20%E0%A4%AE%E0%A5%87%E0%A4%82%20%E0%A4%B8%E0%A4%B9%E0%A4%BE%E0%A4%AF%E0%A4%A4%E0%A4%BE%20%E0%A4%95%E0%A5%80%20%E0%A4%86%E0%A4%B5%E0%A4%B6%E0%A5%8D%E0%A4%AF%E0%A4%95%E0%A4%A4%E0%A4%BE%20%E0%A4%B9%E0%A5%8B%E0%A4%97%E0%A5%80" <> phone <> dl <> rc
  pure NoAction
  ]

eval (SelectButton index) state = continue state { props { buttonIndex = Just index}}

eval OpenAcModal state = continue state { props { acModal = true}}

eval (RequestInfoCardAction RequestInfoCard.Close) state = continue state { props { acModal = false}}

eval (RequestInfoCardAction RequestInfoCard.BackPressed) state = continue state { props { acModal = false}}

eval OpenAmbulanceFacilityModal state = continue state {props {ambulanceModal = true}}

eval (RequestInfoCardAction RequestInfoCard.Close) state = continue state { props { acModal = false}}

eval (RequestInfoCardAction RequestInfoCard.BackPressed) state = continue state { props { acModal = false}}

eval (RequestAmbulanceFacility RequestInfoCard.Close) state = continue state { props { ambulanceModal = false}}

eval (RequestAmbulanceFacility RequestInfoCard.BackPressed) state = continue state { props { ambulanceModal = false}}

eval (SelectAmbulanceVarient varient) state = do
  let {airConditioned , ventilator , oxygen} = case varient of
                                                "AMBULANCE_TAXI" -> { airConditioned: Just false , ventilator: Just false , oxygen: Just false}
                                                "AMBULANCE_TAXI_OXY" -> {airConditioned: Just false , ventilator: Just false , oxygen: Just true}
                                                "AMBULANCE_AC" -> {airConditioned: Just true , ventilator: Just false , oxygen: Just false}
                                                "AMBULANCE_AC_OXY" ->{ airConditioned: Just true , ventilator: Just false , oxygen: Just true}
                                                "AMBULANCE_VENTILATOR" -> {airConditioned: Just true , ventilator: Just true , oxygen: Just true}
                                                _ -> {airConditioned: Nothing , ventilator: Nothing , oxygen: Nothing}
  continue state{data{airConditioned =  airConditioned  , ventilator = ventilator , oxygen = oxygen} , props{isvariant = varient , facilities = not state.props.facilities , showIssueOptions = true}}

eval SelectAmbulanceFacility state = do
  let old = state.props.facilities
  _ <- pure $ hideKeyboardOnNavigation true
  continue state{props{facilities = not old , showIssueOptions = true}}


eval ListExpandAinmationEnd state = continue state {props {showIssueOptions = false }}

eval _ state = update state

checkRegNum :: String -> Boolean
checkRegNum temp = if (length temp > 1) then true else false

overrides :: String -> (Action -> Effect Unit) -> AddVehicleDetailsScreenState -> Props (Effect Unit)
overrides _ push state = []

dateFormat :: Int -> String
dateFormat date = if date < 10 then "0" <> (show date) else (show date)