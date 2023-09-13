{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.UploadDrivingLicenseScreen.Controller where

import Prelude (pure, (==), unit, ($), class Show, bind, discard, (<), (<>), show, (+), (/=), (/), (&&),not)
import PrestoDOM (Eval, continue, exit, continueWithCmd, updateAndExit,toast)
import Debug (spy)
import Screens.Types (UploadDrivingLicenseState)
import Components.RegistrationModal as RegistrationModalController
import Components.OnboardingHeader as OnboardingHeaderController
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Components.GenericMessageModal as GenericMessageModal
import PrestoDOM.Types.Core (class Loggable)
import Components.TutorialModal as TutorialModalController
import JBridge (disableActionEditText, uploadFile, hideKeyboardOnNavigation, openWhatsAppSupport, renderCameraProfilePicture, renderBase64Image)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Effect.Class (liftEffect)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import Data.Maybe
import Helpers.Utils (renderBase64ImageFile)
import Components.StepsHeaderModel.Controller as StepsHeaderModelController
import Components.PopUpModal.Controller as PopUpModal
import Components.ValidateDocumentModal.Controller as ValidateDocumentModal
import Components.PrimaryButton.Controller as PrimaryButtonController


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
    PrimaryEditTextActionControllerReEnter act -> case act of
      PrimaryEditText.TextChanged valId newVal -> trackAppTextInput appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "reenter_dl_number_text_changed" "primary_edit_text"
      PrimaryEditText.FocusChanged _ -> trackAppTextInput appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "reenter_dl_number_text_focus_changed" "primary_edit_text"
    CallBackImageUpload str imageName imagePath -> trackAppScreenEvent appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "call_back_image_upload"
    DatePicker (label) year month date -> do
      if label == "DATE_OF_BIRTH" then trackAppScreenEvent appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "date_of_birth"
        else if label == "DATE_OF_ISSUE" then trackAppScreenEvent appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "date_of_issue"
          else trackAppScreenEvent appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "no_action"
    PreviewAction -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "preview"
    SelectDateOfBirthAction -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "select_date_of_birth"
    SelectDateOfIssueAction -> trackAppActionClick appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "select_date_of_issue"
    NoAction -> trackAppScreenEvent appId (getScreen UPLOAD_DRIVING_LICENSE_SCREEN) "in_screen" "no_action"
    StepsHeaderModelAC act -> case act of
      StepsHeaderModelController.OnArrowClick -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "in_screen" "steps_header_on_click"
      StepsHeaderModelController.Logout -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "in_screen" "steps_header_logout"
    PopUpModalLogoutAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal" "on_goback"
      PopUpModal.Tipbtnclick _ _ -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal" "tip_button_click"
      PopUpModal.DismissPopup -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal" "dismiss_popup"
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal" "call_support"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal_action" "no_action"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal_action" "image"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen REGISTRATION_SCREEN) "popup_modal_action" "primary_edit_text"
      PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "popup_modal_action" "countdown_updated"
    ValidateDocumentModalAction act -> case act of
      ValidateDocumentModal.BackPressed  -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "validate_document_modal" "backpressed"
      ValidateDocumentModal.AfterRender ->  trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "validate_document_modal" "afterrender"
      _ -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "validate_document_modal" "no_action"
    PopUpModalActions act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal" "on_goback"
      PopUpModal.Tipbtnclick _ _ -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal" "tip_button_click"
      PopUpModal.DismissPopup -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal" "dismiss_popup"
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal" "call_support"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal_action" "no_action"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal_action" "image"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen REGISTRATION_SCREEN) "popup_modal_action" "primary_edit_text"
      PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "popup_modal_action" "countdown_updated"
    RenderProfileImage image id -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "renderImage" "afterrender"

data ScreenOutput = GoBack UploadDrivingLicenseState
                    | GoToAddVehicleDetailsScreen UploadDrivingLicenseState 
                    | ValidateImageAPICall UploadDrivingLicenseState 
                    | AddVehicleDetailsScreen
                    | LogoutAccount
      
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
            | DatePicker String Int Int Int
            | PrimaryEditTextActionController PrimaryEditText.Action 
            | PrimaryEditTextActionControllerReEnter PrimaryEditText.Action 
            | GenericMessageModalAction GenericMessageModal.Action
            | PreviewAction
            | SelectDateOfBirthAction
            | SelectDateOfIssueAction
            | StepsHeaderModelAC StepsHeaderModelController.Action
            | PopUpModalLogoutAction PopUpModal.Action
            | ValidateDocumentModalAction ValidateDocumentModal.Action
            | RenderProfileImage String String
            | PopUpModalActions PopUpModal.Action

eval :: Action -> UploadDrivingLicenseState -> Eval Action ScreenOutput UploadDrivingLicenseState
eval AfterRender state = 
                 if (state.props.validateProfilePicturePopUp == true) then do 
                  continueWithCmd state [do pure (RenderProfileImage state.data.imageFrontUrl (getNewIDWithTag "ValidateProfileImage"))]  
                 else continue state

eval (RenderProfileImage image id) state = do
  continueWithCmd state [do 
    _ <- liftEffect $ renderBase64ImageFile image id false state.props.fileCameraOption
    pure NoAction]

eval (BackPressed flag) state = do
            if(state.props.validateProfilePicturePopUp) then do
                if (state.props.fileCameraOption) then continueWithCmd (state {props{clickedButtonType = "front", validateProfilePicturePopUp = false,imageCaptureLayoutView = true}}) [ pure UploadImage]
                else continueWithCmd state {props {clickedButtonType = "front", fileCameraPopupModal = false, fileCameraOption = false, validateProfilePicturePopUp = false, imageCaptureLayoutView = false}} [do
                      _ <- liftEffect $ uploadFile false
                      pure NoAction]
            else if(state.props.imageCaptureLayoutView) then continue state{props{imageCaptureLayoutView = false,openHowToUploadManual = true}} 
            else if(state.props.fileCameraPopupModal) then continue state{props{fileCameraPopupModal = false, validateProfilePicturePopUp = false, imageCaptureLayoutView = false}} 
            else if(state.props.openHowToUploadManual) then continue state{props{openHowToUploadManual = false}} 
            else exit $ GoBack state
eval (OnboardingHeaderAction (OnboardingHeaderController.TriggerRegModal)) state = continue state{props{openRegistrationModal = true}}
eval (RegistrationModalAction (RegistrationModalController.OnCloseClick)) state = continue state{props{openRegistrationModal = false}}
eval (PrimaryButtonAction (PrimaryButton.OnClick)) state = do
  _ <- pure $ toast ("HELLO1")
  _ <- pure $ hideKeyboardOnNavigation true
  if (state.props.openHowToUploadManual == false) then 
    continue state {props {openHowToUploadManual = true}}
  else
   continue state {props {fileCameraPopupModal = true }}
    --continueWithCmd (state {props{clickedButtonType = "front"}}) [ pure UploadImage]
    --updateAndExit state $ (GoToAddVehicleDetailsScreen state)
eval (PrimaryEditTextActionController (PrimaryEditText.TextChanged id value)) state = do
  _ <- pure $ disableActionEditText (getNewIDWithTag "EnterDrivingLicenseEditText")
  continue state {data = state.data { driver_license_number = value }}
eval (PrimaryEditTextActionControllerReEnter (PrimaryEditText.TextChanged id value))state = do
  _ <- pure $ disableActionEditText (getNewIDWithTag "ReEnterDrivingLicenseEditText")
  continue state {data = state.data { reEnterDriverLicenseNumber = value }}
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
  _ <- liftEffect $ openWhatsAppSupport "+918618963188"
  pure NoAction
  ]
eval (TutorialModalAction (TutorialModalController.Logout)) state = exit LogoutAccount
eval (RemoveUploadedFile removeType) state = if(removeType == "front") then continue state{data{imageFront = ""}} else continue state{data{imageBack = ""}}
eval (UploadFileAction clickedType) state = continueWithCmd (state {props {clickedButtonType = clickedType}}) [ pure UploadImage]
eval (UploadImage) state = continueWithCmd (state {props {validateProfilePicturePopUp = false, imageCaptureLayoutView = true}}) [do
     _ <- liftEffect $ renderCameraProfilePicture (getNewIDWithTag "ProfilePictureCaptureLayout")
     pure NoAction
      ]--continueWithCmd state [do
 -- _ <- liftEffect $ uploadFile unit
  --pure NoAction]

    --  continueWithCmd state {props {validateProfilePicturePopUp = false,imageCaptureLayoutView = true}} [ do
    --  _ <- liftEffect $ renderCameraProfilePicture (getNewIDWithTag "ProfilePictureCaptureLayout")
    --  pure NoAction
    --  ]
eval (GenericMessageModalAction (GenericMessageModal.PrimaryButtonActionController (PrimaryButton.OnClick))) state = do
   exit $ GoBack state {props {openGenericMessageModal = false}}


eval (CallBackImageUpload image imageName imagePath) state = if(state.props.clickedButtonType == "front") then do
                                                      updateAndExit  state {data {imageFrontUrl = image, imageFront = image, imageNameFront = imageName}} $ ValidateImageAPICall $ state {data {imageFrontUrl = image, imageFront = image, imageNameFront = imageName}}
                                                        else if(state.props.clickedButtonType == "back") then do
                                                          updateAndExit state {data {imageBack = image, imageNameBack = imageName}} $ ValidateImageAPICall $ state {data {imageBack = image, imageNameBack = imageName}}
                                                        else do
                                                           continue state                    

eval (DatePicker (label) year month date) state = do
  case label of
    "DATE_OF_BIRTH" -> continue state {data = state.data { dob = (dateFormat year) <> "-" <> (dateFormat (month+1)) <> "-" <> (dateFormat date) <> " 00:00:00.233691+00" , dobView = (show date) <> "/" <> (show (month+1)) <> "/" <> (show year)}}
    "DATE_OF_ISSUE" -> continue state {data = state.data { dateOfIssue = Just $ (dateFormat year) <> "-" <> (dateFormat (month+1)) <> "-" <> (dateFormat date) <> " 00:00:00.233691+00" , dateOfIssueView = (show date) <> "/" <> (show (month+1)) <> "/" <> (show year), imageFront = "null"}} -- imageFront made null to handle fallback
    _ -> continue state

eval (PopUpModalLogoutAction (PopUpModal.OnButton2Click)) state = continue $ (state {props {logoutPopupModal= false}})

eval (PopUpModalLogoutAction (PopUpModal.OnButton1Click)) state = exit $ LogoutAccount

eval (StepsHeaderModelAC (StepsHeaderModelController.Logout)) state = continue $ (state {props{logoutPopupModal = true}})

eval (StepsHeaderModelAC StepsHeaderModelController.OnArrowClick) state = continueWithCmd state{props{openLicenseManual = false}} [ do pure $ BackPressed false]

eval (ValidateDocumentModalAction (ValidateDocumentModal.AfterRender)) state = do
 continueWithCmd state [do pure (AfterRender)] 

eval (ValidateDocumentModalAction (ValidateDocumentModal.BackPressed)) state = do
 continueWithCmd state [do pure (BackPressed false)]  

eval (ValidateDocumentModalAction (ValidateDocumentModal.PrimaryButtonActionController (PrimaryButtonController.OnClick))) state = do
   if (not state.props.errorVisibility) then do
     exit $ GoToAddVehicleDetailsScreen state {  props {validateProfilePicturePopUp = false,imageCaptureLayoutView=false}} 
   else 
     continueWithCmd state [ pure (BackPressed false)]
 
eval (PopUpModalActions (PopUpModal.OnButton2Click)) state = do
   continueWithCmd state {props {clickedButtonType = "front", fileCameraPopupModal = false, fileCameraOption = false}} [do
     _ <- liftEffect $ uploadFile false
     pure NoAction]

eval (PopUpModalActions (PopUpModal.OnButton1Click)) state = do
       continueWithCmd (state {props{clickedButtonType = "front", validateProfilePicturePopUp = false,imageCaptureLayoutView = true, fileCameraPopupModal = false, fileCameraOption = true}}) [ pure UploadImage]
    
eval _ state = continue state

dateFormat :: Int -> String
dateFormat date = if date < 10 then "0" <> (show date) else (show date)
