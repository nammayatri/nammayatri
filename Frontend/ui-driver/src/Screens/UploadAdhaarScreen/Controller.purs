{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.UploadAdhaarScreen.Controller where

import Prelude (pure, (==), unit, ($), class Show, bind, discard)
import PrestoDOM (Eval, continue, exit, continueWithCmd)
import Screens.Types (UploadAdhaarScreenState)
import Components.RegistrationModal as RegistrationModalController
import Components.PrimaryButton as PrimaryButton
import Components.OnboardingHeader as OnboardingHeaderController
import PrestoDOM.Types.Core (class Loggable)
import Effect.Class (liftEffect)
import JBridge (uploadFile)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen UPLOAD_AADHAR_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen UPLOAD_AADHAR_SCREEN)
      trackAppEndScreen appId (getScreen UPLOAD_AADHAR_SCREEN)
    RegistrationModalAction (RegistrationModalController.OnCloseClick) -> trackAppActionClick appId (getScreen UPLOAD_AADHAR_SCREEN) "registration_modal" "on_close_click"
    OnboardingHeaderAction act -> case act of
      OnboardingHeaderController.TriggerRegModal -> trackAppActionClick appId (getScreen UPLOAD_AADHAR_SCREEN) "onboarding_header" "trigger_registration_modal"
      OnboardingHeaderController.BackPressed -> do
        trackAppActionClick appId (getScreen UPLOAD_AADHAR_SCREEN) "onboarding_header" "backpressed"
        trackAppEndScreen appId (getScreen UPLOAD_AADHAR_SCREEN)
    PrimaryButtonAction act -> case act of
      PrimaryButton.OnClick -> do
        trackAppActionClick appId (getScreen UPLOAD_AADHAR_SCREEN) "primary_button" "next_on_click"
        trackAppEndScreen appId (getScreen UPLOAD_AADHAR_SCREEN)
      PrimaryButton.NoAction -> trackAppActionClick appId (getScreen UPLOAD_AADHAR_SCREEN) "primary_button" "no_action"
    RemoveUploadedFile str -> trackAppActionClick appId (getScreen UPLOAD_AADHAR_SCREEN) "in_screen" "remove_uploaded_file"
    UploadFileAction str -> trackAppActionClick appId (getScreen UPLOAD_AADHAR_SCREEN) "'in_screen" "upload_file_action"
    UploadImage -> trackAppActionClick appId (getScreen UPLOAD_AADHAR_SCREEN) "in_screen" "upload_image_onclick"
    CallBackImageUpload str imageName imagePath -> trackAppScreenEvent appId (getScreen UPLOAD_AADHAR_SCREEN) "in_screen" "call_back_image_upload"
    PreviewImageAction -> trackAppActionClick appId (getScreen UPLOAD_AADHAR_SCREEN) "in_screen" "preview"
    NoAction -> trackAppScreenEvent appId (getScreen UPLOAD_AADHAR_SCREEN) "in_screen" "no_action"

data ScreenOutput = GoBack | GoToBankDetails UploadAdhaarScreenState
data Action = BackPressed 
            | NoAction
            | AfterRender
            | RegistrationModalAction RegistrationModalController.Action
            | OnboardingHeaderAction OnboardingHeaderController.Action
            | PrimaryButtonAction PrimaryButton.Action
            | RemoveUploadedFile String
            | CallBackImageUpload String String String
            | UploadFileAction String
            | UploadImage
            | PreviewImageAction

eval :: Action -> UploadAdhaarScreenState -> Eval Action ScreenOutput UploadAdhaarScreenState
eval AfterRender state = continue state
eval BackPressed state = exit GoBack
eval (OnboardingHeaderAction (OnboardingHeaderController.BackPressed)) state = exit GoBack
eval (OnboardingHeaderAction (OnboardingHeaderController.TriggerRegModal)) state = continue state{props{openRegistrationModal = true}}
eval (RegistrationModalAction (RegistrationModalController.OnCloseClick)) state = continue state{props{openRegistrationModal = false}}
eval (PrimaryButtonAction (PrimaryButton.OnClick)) state = exit (GoToBankDetails state)
eval (RemoveUploadedFile removeType) state = if(removeType == "front") then continue state{data{imageFront = ""}} else continue state{data{imageBack = ""}}
eval (UploadFileAction clickedType) state = continueWithCmd (state {props {clickedButtonType = clickedType}}) [ pure UploadImage]
eval (UploadImage) state = continueWithCmd state [do
  _ <- liftEffect $ uploadFile false
  pure NoAction]
eval (CallBackImageUpload image imageName imagePath) state = if(state.props.clickedButtonType == "front") then continue $ state {data {imageFront = image}}
                                            else if(state.props.clickedButtonType == "back") then continue $ state {data {imageBack = image}}
                                              else continue state
                                              
eval _ state = continue state



