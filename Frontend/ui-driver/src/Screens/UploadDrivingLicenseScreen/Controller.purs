{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.UploadDrivingLicenseScreen.Controller where

import Data.Maybe

import Common.Types.App (LazyCheck(..), UploadFileConfig(..))
import Components.GenericMessageModal as GenericMessageModal
import Components.OnboardingHeader as OnboardingHeaderController
import Components.PopUpModal.Controller as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Components.RegistrationModal as RegistrationModalController
import Components.TutorialModal as TutorialModalController
import Components.ValidateDocumentModal.Controller as ValidateDocumentModal
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import Components.GenericHeader as GenericHeader
import Data.String (length, toUpper)
import Debug (spy)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.Commons (getNewIDWithTag, setText)
import Engineering.Helpers.LogEvent (logEvent)
import Helpers.Utils (contactSupportNumber)
import JBridge (disableActionEditText, hideKeyboardOnNavigation, openWhatsAppSupport, renderBase64Image, renderCameraProfilePicture, showDialer, uploadFile, renderBase64ImageFile)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude (pure, (==), unit, void,  ($), class Show, bind, discard, (<), (<>), show, (+), (/=), (/), (&&), not)
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, toast, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (UploadDrivingLicenseState)
import Services.Config (getSupportNumber, getWhatsAppSupportNo)
import Storage (KeyStore(..), getValueToLocalStore)
import Effect.Uncurried (runEffectFn4)
import Components.OptionsMenu as OptionsMenu
import Components.BottomDrawerList as BottomDrawerList
import Screens.Types as ST
import JBridge as JB
import Engineering.Helpers.Events as EHE


instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen UPLOAD_DRIVING_LICENSE_SCREEN)
    BackPressed flag -> do
      trackAppBackPress appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN)
      if flag then trackAppScreenEvent appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "backpress_in_open_license_manual"
        else trackAppEndScreen appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN)
    RegistrationModalAction (RegistrationModalController.OnCloseClick) -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "registration_modal" "on_close_click"
    OnboardingHeaderAction act -> case act of
      OnboardingHeaderController.TriggerRegModal -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "onboarding_header" "trigger_registration_modal"
      OnboardingHeaderController.BackPressed -> do
        trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "onboarding_header" "backpressed"
        trackAppEndScreen appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN)
    PrimaryButtonAction act -> case act of
      PrimaryButton.OnClick -> do
        trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "primary_button" "next_on_click"
        trackAppEndScreen appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN)
      PrimaryButton.NoAction -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "primary_button" "no_action"
    DriverLicenseManual -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "driver_license_manual_clicked"
    TutorialModalAction act -> case act of
      TutorialModalController.OnCloseClick -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "tutorial_modal" "on_close_click"
      TutorialModalController.CallSupport -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "call_support_onclick"
      TutorialModalController.Logout -> do
        trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "Logout"
        trackAppEndScreen appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN)
    TutorialModal manual -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "tutorial_modal_clicked"
    RemoveUploadedFile str -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "remove_uploaded_file"
    UploadFileAction str -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "'in_screen" "upload_file"
    UploadImage -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "upload_image"
    GenericMessageModalAction (GenericMessageModal.PrimaryButtonActionController act) -> case act of 
      PrimaryButton.OnClick -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "generic_message_modal" "primary_button_next_on_click"
      PrimaryButton.NoAction -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "generic_message_modal" "primary_button_next_no_action"
    PrimaryEditTextActionController act -> case act of
      PrimaryEditText.TextChanged valId newVal -> trackAppTextInput appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "dl_number_text_changed" "primary_edit_text"
      PrimaryEditText.FocusChanged _ -> trackAppTextInput appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "dl_number_text_focus_changed" "primary_edit_text"
      PrimaryEditText.TextImageClicked -> trackAppTextInput appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "dl_number_text_image_clicked" "primary_edit_text"
    PrimaryEditTextActionControllerReEnter act -> case act of
      PrimaryEditText.TextChanged valId newVal -> trackAppTextInput appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "reenter_dl_number_text_changed" "primary_edit_text"
      PrimaryEditText.FocusChanged _ -> trackAppTextInput appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "reenter_dl_number_text_focus_changed" "primary_edit_text"
      PrimaryEditText.TextImageClicked -> trackAppTextInput appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "reenter_dl_number_text_image_clicked" "primary_edit_text"
    CallBackImageUpload str imageName imagePath -> trackAppScreenEvent appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "call_back_image_upload"
    DatePicker (label) resp year month date -> do
      if label == "DATE_OF_BIRTH" then trackAppScreenEvent appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "date_of_birth"
        else if label == "DATE_OF_ISSUE" then trackAppScreenEvent appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "date_of_issue"
          else trackAppScreenEvent appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "no_action"
    PreviewAction -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "preview"
    SelectDateOfBirthAction -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "select_date_of_birth"
    SelectDateOfIssueAction -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "select_date_of_issue"
    NoAction -> trackAppScreenEvent appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "no_action"
    PopUpModalLogoutAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "popup_modal" "on_goback"
      PopUpModal.DismissPopup -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "popup_modal" "dismiss_popup"
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "popup_modal" "call_support"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "popup_modal_action" "no_action"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "popup_modal_action" "image"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "popup_modal_action" "primary_edit_text"
      PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "popup_modal_action" "countdown_updated"
      _ -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "popup_modal_action" "no_action"
    ValidateDocumentModalAction act -> case act of
      ValidateDocumentModal.BackPressed  -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "validate_document_modal" "backpressed"
      ValidateDocumentModal.AfterRender ->  trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "validate_document_modal" "afterrender"
      _ -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "validate_document_modal" "no_action"
    PopUpModalActions act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "popup_modal" "on_goback"
      PopUpModal.DismissPopup -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "popup_modal" "dismiss_popup"
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "popup_modal" "call_support"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "popup_modal_action" "no_action"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "popup_modal_action" "image"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "popup_modal_action" "primary_edit_text"
      PopUpModal.CountDown arg1 arg2 arg3 -> trackAppScreenEvent appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "popup_modal_action" "countdown_updated"
      _ -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "popup_modal_action" "no_action"
    RenderProfileImage image id -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "renderImage" "afterrender"
    RedirectScreen -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "redirect_screen" "no_action"
    AppOnboardingNavBarAC act -> case act of
      AppOnboardingNavBar.GenericHeaderAC genericHeaderAction -> case genericHeaderAction of 
        GenericHeader.PrefixImgOnClick -> trackAppScreenEvent appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "generic_header_on_click"
        GenericHeader.SuffixImgOnClick -> trackAppScreenEvent appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "generic_header_on_click"
      AppOnboardingNavBar.Logout -> trackAppScreenEvent appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "onboarding_nav_bar_logout"
      AppOnboardingNavBar.PrefixImgOnClick -> trackAppScreenEvent appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "app_onboarding_nav_bar_prefix_img_on_click"
    SkipButton -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "skip_button_click"
    _ -> pure unit

data ScreenOutput = GoBack UploadDrivingLicenseState
                    | ValidateDetails UploadDrivingLicenseState 
                    | ValidateDataCall UploadDrivingLicenseState 
                    | AddVehicleDetailsScreen
                    | LogoutAccount
                    | GoToRegisteration
                    | ChangeVehicle UploadDrivingLicenseState
                    | SelectLang UploadDrivingLicenseState
                    | GoToFaqsScreen UploadDrivingLicenseState
      
data Action = BackPressed Boolean
            | NoAction
            | AfterRender
            | RegistrationModalAction RegistrationModalController.Action
            | OnboardingHeaderAction OnboardingHeaderController.Action
            | PrimaryButtonAction PrimaryButton.Action
            | DriverLicenseManual
            | TutorialModalAction TutorialModalController.Action
            | TutorialModal String
            | RemoveUploadedFile String
            | CallBackImageUpload String String String
            | UploadFileAction String
            | UploadImage
            | DatePicker String String Int Int Int
            | PrimaryEditTextActionController PrimaryEditText.Action 
            | PrimaryEditTextActionControllerReEnter PrimaryEditText.Action 
            | GenericMessageModalAction GenericMessageModal.Action
            | PreviewAction
            | SelectDateOfBirthAction
            | SelectDateOfIssueAction
            | PopUpModalLogoutAction PopUpModal.Action
            | ValidateDocumentModalAction ValidateDocumentModal.Action
            | RenderProfileImage String String
            | PopUpModalActions PopUpModal.Action
            | RedirectScreen
            | AppOnboardingNavBarAC AppOnboardingNavBar.Action
            | SkipButton
            | OptionsMenuAction OptionsMenu.Action
            | ChangeVehicleAC PopUpModal.Action
            | BottomDrawerListAC BottomDrawerList.Action
            | WhatsAppClick

uploadFileConfig :: UploadFileConfig
uploadFileConfig = UploadFileConfig {
  showAccordingToAspectRatio : false,
  imageAspectHeight : 0,
  imageAspectWidth : 0
}

eval :: Action -> UploadDrivingLicenseState -> Eval Action ScreenOutput UploadDrivingLicenseState
eval AfterRender state = 
                 if state.props.validateProfilePicturePopUp then do 
                  continueWithCmd state [do pure (RenderProfileImage state.data.imageFrontUrl (getNewIDWithTag "ValidateProfileImage"))]  
                 else continue state

eval (RenderProfileImage image id) state = do
  continueWithCmd state [do 
    void $ liftEffect $ runEffectFn4 renderBase64ImageFile image id false "CENTER_CROP"
    pure NoAction]

eval (BackPressed flag) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  if(state.props.validateProfilePicturePopUp) then do
      if (state.props.fileCameraOption) then continueWithCmd (state {props{clickedButtonType = "front", validateProfilePicturePopUp = false,imageCaptureLayoutView = true}}) [ pure UploadImage]
      else continueWithCmd state {props {clickedButtonType = "front", fileCameraPopupModal = false, fileCameraOption = false, validateProfilePicturePopUp = false, imageCaptureLayoutView = false}} [do
            _ <- liftEffect $ uploadFile uploadFileConfig true
            pure NoAction]
  else if state.props.imageCaptureLayoutView then continue state{props{imageCaptureLayoutView = false,openHowToUploadManual = true}} 
  else if state.props.fileCameraPopupModal then continue state{props{fileCameraPopupModal = false, validateProfilePicturePopUp = false, imageCaptureLayoutView = false}} 
  else if state.props.openHowToUploadManual then continue state{props{openHowToUploadManual = false}} 
  else if state.props.logoutPopupModal then continue state{props{logoutPopupModal = false}} 
  else if state.props.confirmChangeVehicle then continue state{props{confirmChangeVehicle = false}} 
  else if state.props.menuOptions then continue state{props{menuOptions = false}} 
  else if state.props.contactSupportModal == ST.SHOW then continue state { props { contactSupportModal = ST.ANIMATING}}
  else exit $ GoBack state
    
eval (OnboardingHeaderAction (OnboardingHeaderController.TriggerRegModal)) state = continue state{props{openRegistrationModal = true}}

eval (RegistrationModalAction (RegistrationModalController.OnCloseClick)) state = continue state{props{openRegistrationModal = false}}

eval (PrimaryButtonAction (PrimaryButton.OnClick)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  if isJust state.data.dateOfIssue then  exit $ ValidateDataCall state
  else if (not state.props.openHowToUploadManual) then do
    let _ = EHE.addEvent (EHE.defaultEventObject "upload_dl_clicked") 
    let _ = EHE.addEvent (EHE.defaultEventObject "upload_dl_page_loaded")
    continue state {props {openHowToUploadManual = true}}
  else
    continueWithCmd state {props {clickedButtonType = "front", fileCameraPopupModal = false, fileCameraOption = false}} [do
     let _ = EHE.addEvent (EHE.defaultEventObject "upload_dl_photo_clicked")
     _ <- liftEffect $ uploadFile uploadFileConfig true
     pure NoAction]

eval (PrimaryEditTextActionController (PrimaryEditText.TextChanged id value)) state = do
  _ <- pure $ disableActionEditText (getNewIDWithTag "EnterDrivingLicenseEditText")
  if (length value == 16) then do 
    let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_dl_entry"
    pure unit
    else pure unit
  continue state {data = state.data { driver_license_number = toUpper value }}

eval (PrimaryEditTextActionControllerReEnter (PrimaryEditText.TextChanged id value))state = do
  _ <- pure $ disableActionEditText (getNewIDWithTag "ReEnterDrivingLicenseEditText")
  if (length value == 16) then do 
    let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_dl_re_entry"
    pure unit
    else pure unit
  continue state {data = state.data { reEnterDriverLicenseNumber = toUpper value }}

eval DriverLicenseManual state = continue state{props{openLicenseManual = true}}

eval (OnboardingHeaderAction (OnboardingHeaderController.BackPressed)) state = exit $ GoBack state

eval (TutorialModal manual) state = do
  pure $ hideKeyboardOnNavigation true 
  case manual of 
    "DATE_OF_ISSUE" -> continue state{props{openDateOfIssueManual = true} }
    "LICENSE" -> continue state{props{openLicenseManual = true}}
    _ -> continue state
eval (TutorialModalAction (TutorialModalController.OnCloseClick)) state = continue state{props{openLicenseManual = false, openDateOfIssueManual = false}} 

eval (TutorialModalAction (TutorialModalController.CallSupport)) state = continueWithCmd state [do
  let merchant = getMerchant FunctionCall
  _ <- case merchant of
    NAMMAYATRI -> do 
      _ <- contactSupportNumber "WHATSAPP"
      pure unit
    YATRISATHI -> openWhatsAppSupport $ getWhatsAppSupportNo $ show merchant
    _ -> pure $ showDialer (getSupportNumber "") false
  pure NoAction
  ]
eval (TutorialModalAction (TutorialModalController.Logout)) state = exit LogoutAccount
eval (RemoveUploadedFile removeType) state = if(removeType == "front") then continue state{data{imageFront = ""}} else continue state{data{imageBack = ""}}
eval (UploadFileAction clickedType) state = continueWithCmd (state {props {clickedButtonType = clickedType}}) [ pure UploadImage]
eval (UploadImage) state = continueWithCmd (state {props {validateProfilePicturePopUp = false, imageCaptureLayoutView = true}}) [do
     _ <- liftEffect $ renderCameraProfilePicture (getNewIDWithTag "ProfilePictureCaptureLayout")
     pure NoAction
      ]
      
eval (GenericMessageModalAction (GenericMessageModal.PrimaryButtonActionController (PrimaryButton.OnClick))) state = do
   exit $ GoBack state {props {openGenericMessageModal = false}}


eval (CallBackImageUpload image imageName imagePath) state = 
  if(state.props.clickedButtonType == "front") then do
    let newState = state {data {imageFrontUrl = image, imageFront = image, imageNameFront = imageName}, props{validateProfilePicturePopUp = true, imageCaptureLayoutView = false}}
    continueWithCmd newState [ do
      void $ runEffectFn4 renderBase64ImageFile image (getNewIDWithTag "ValidateProfileImage") false "CENTER_CROP"
      pure $ ValidateDocumentModalAction (ValidateDocumentModal.PrimaryButtonActionController (PrimaryButton.OnClick))]
  else if(state.props.clickedButtonType == "back") then do
    continue state {data {imageBack = image, imageNameBack = imageName}, props{validateProfilePicturePopUp = true, imageCaptureLayoutView = false}}
  else do
    continue state                    

eval (DatePicker (label) resp year month date) state = do
  let fullDate = (dateFormat year) <> "-" <> (dateFormat (month+1)) <> "-" <> (dateFormat date) <> " 00:00:00.233691+00" 
  let dateView = (show date) <> "/" <> (show (month+1)) <> "/" <> (show year)
  case resp of 
    "SELECTED" -> case label of
                    "DATE_OF_BIRTH" -> do
                      let _ = unsafePerformEffect $ logEvent state.data.logField "NY Driver - DOB"
                          _ = EHE.addEvent (EHE.defaultEventObject "dl_dob_entered") {payload = ""}
                      continue state {data = state.data { dob = fullDate, dobView = dateView }
                                                        , props {isDateClickable = true }}
                    "DATE_OF_ISSUE" -> continue state {data = state.data { dateOfIssue = Just fullDate , dateOfIssueView = dateView, imageFront = "null"}
                                                        , props {isDateClickable = true }} 
                    _ -> continue state { props {isDateClickable = true}}
    _ -> continue state { props {isDateClickable = true}}

eval SelectDateOfBirthAction state = continue state { props {isDateClickable = false}}

eval SelectDateOfIssueAction state = continue state { props {isDateClickable = false}} 


eval (PopUpModalLogoutAction (PopUpModal.OnButton2Click)) state = continue $ (state {props {logoutPopupModal= false}})

eval (PopUpModalLogoutAction (PopUpModal.OnButton1Click)) state = exit $ LogoutAccount

eval (PopUpModalLogoutAction (PopUpModal.DismissPopup)) state = continue state {props {logoutPopupModal= false}}

eval (AppOnboardingNavBarAC (AppOnboardingNavBar.Logout)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  continue state {props{menuOptions = not state.props.menuOptions}}

eval (AppOnboardingNavBarAC (AppOnboardingNavBar.PrefixImgOnClick) ) state = continueWithCmd state{props{openLicenseManual = false}} [ do pure $ BackPressed false]

eval (ValidateDocumentModalAction (ValidateDocumentModal.AfterRender)) state = do
 continueWithCmd state [do pure (AfterRender)] 

eval (ValidateDocumentModalAction (ValidateDocumentModal.BackPressed)) state = do
 continueWithCmd state [do pure (BackPressed false)]

eval SkipButton state = exit $ ValidateDataCall state

eval (ValidateDocumentModalAction (ValidateDocumentModal.PrimaryButtonActionController (PrimaryButton.OnClick))) state = do
   if (not state.props.errorVisibility) then do
     updateAndExit state{props{validating = true}} $ ValidateDetails state{props{validating = true}}
   else 
     continueWithCmd state {props {validateProfilePicturePopUp = false, errorVisibility = false, clickedButtonType = "front", fileCameraPopupModal = false, fileCameraOption = false}, data{errorMessage = ""}} [do
     void $ liftEffect $ uploadFile uploadFileConfig true
     pure NoAction]
 
eval (PopUpModalActions (PopUpModal.OnButton2Click)) state = do
   continueWithCmd state {props {clickedButtonType = "front", fileCameraPopupModal = false, fileCameraOption = false}} [do
     void $ liftEffect $ uploadFile uploadFileConfig true
     pure NoAction]

eval (PopUpModalActions (PopUpModal.OnButton1Click)) state = do
       continueWithCmd (state {props{clickedButtonType = "front", validateProfilePicturePopUp = false,imageCaptureLayoutView = true, fileCameraPopupModal = false, fileCameraOption = true}}) [ pure UploadImage]
    
eval RedirectScreen state = exit GoToRegisteration

eval (OptionsMenuAction OptionsMenu.BackgroundClick) state = continue state{props{menuOptions = false}}

eval (OptionsMenuAction (OptionsMenu.ItemClick item)) state = do
  let newState = state{props{menuOptions = false}}
  case item of
    "logout" -> continue newState {props { logoutPopupModal = true }}
    "contact_support" -> continue newState { props { contactSupportModal = ST.SHOW}}
    "change_vehicle" -> continue newState {props {confirmChangeVehicle = true}}
    "change_language" -> exit $ SelectLang newState
    "faqs" -> exit $ GoToFaqsScreen newState
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

eval _ state = update state

dateFormat :: Int -> String
dateFormat date = if date < 10 then "0" <> (show date) else (show date)
