{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DocumentCaptureScreen.Controller where 

import Components.GenericHeader.Controller (Action(..)) as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Prelude (class Show, pure, unit, bind, discard, ($), (/=), (==), void)
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (DocumentCaptureScreenState)
import Effect.Class (liftEffect)
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import Components.ValidateDocumentModal.Controller as ValidateDocumentModal
import JBridge as JB
import Log (printLog)
import Effect.Uncurried (runEffectFn4)
import Engineering.Helpers.Commons as EHC
import Components.PopUpModal.Controller as PopUpModal
import Data.String as DS
import Data.Maybe (Maybe(..), isNothing)
import Components.OptionsMenu as OptionsMenu
import Services.Config as SC

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    _ -> pure unit

data Action = PrimaryButtonAC PrimaryButtonController.Action 
            | GenericHeaderAC GenericHeaderController.Action
            | AppOnboardingNavBarAC AppOnboardingNavBar.Action
            | CallBackImageUpload String String String
            | ValidateDocumentModalAction ValidateDocumentModal.Action
            | PopUpModalLogoutAction PopUpModal.Action
            | OptionsMenuAction OptionsMenu.Action
            | BackPressed
            | NoAction
            | ChangeVehicleAC PopUpModal.Action

data ScreenOutput = GoBack 
                  | UploadAPI DocumentCaptureScreenState
                  | LogoutAccount
                  | SelectLang DocumentCaptureScreenState
                  | ChangeVehicle DocumentCaptureScreenState

eval :: Action -> DocumentCaptureScreenState -> Eval Action ScreenOutput DocumentCaptureScreenState

eval (PrimaryButtonAC PrimaryButtonController.OnClick) state = continueWithCmd state [do
  _ <- liftEffect $ JB.uploadFile false
  pure NoAction]

eval (AppOnboardingNavBarAC (AppOnboardingNavBar.Logout)) state = continue state {props { menuOptions = true }}

eval (AppOnboardingNavBarAC AppOnboardingNavBar.PrefixImgOnClick) state = continueWithCmd state [pure BackPressed]

eval (CallBackImageUpload imageBase64 imageName imagePath) state = do
  if imageBase64 /= "" then continueWithCmd state { data { imageBase64 = imageBase64 }, props { validateDocModal = true}} [ do
      void $ runEffectFn4 JB.renderBase64ImageFile imageBase64 (EHC.getNewIDWithTag "ValidateProfileImage") false "CENTER_CROP"
      pure $ NoAction]
  else continue state

eval (ValidateDocumentModalAction (ValidateDocumentModal.BackPressed)) state = continueWithCmd state [pure BackPressed]  

eval (ValidateDocumentModalAction (ValidateDocumentModal.PrimaryButtonActionController (PrimaryButtonController.OnClick))) state = 
  if isNothing state.data.errorMessage then
    updateAndExit state{props{validating = true}} $ UploadAPI state{props{validating = true}}
  else 
    continueWithCmd state {props {validateDocModal = false}, data{errorMessage = Nothing}} [do
    void $ liftEffect $ JB.uploadFile false
    pure NoAction]

eval (PopUpModalLogoutAction (PopUpModal.OnButton2Click)) state = continue $ (state {props {logoutModalView= false}})

eval (PopUpModalLogoutAction (PopUpModal.OnButton1Click)) state = exit $ LogoutAccount

eval (ValidateDocumentModalAction (ValidateDocumentModal.AfterRender)) state = continueWithCmd state [pure (CallBackImageUpload state.data.imageBase64 "" "")]

eval BackPressed state = 
  if state.props.validateDocModal then continue state { props { validateDocModal = false}}
  else if state.props.logoutModalView then continue state { props { logoutModalView = false}}
  else if state.props.confirmChangeVehicle then continue state{props{confirmChangeVehicle = false}}
  else if state.props.menuOptions then continue state{props{menuOptions = false}} 
  else exit $ GoBack

eval (OptionsMenuAction OptionsMenu.BackgroundClick) state = continue state{props{menuOptions = false}}

eval (OptionsMenuAction (OptionsMenu.ItemClick item)) state = do
  let newState = state{props{menuOptions = false}}
  case item of
    "logout" -> continue newState {props { logoutModalView = true }}
    "contact_support" -> do 
                          void $ pure $ JB.showDialer (SC.getSupportNumber "") false 
                          continue newState
    "change_vehicle" -> continue newState {props {confirmChangeVehicle = true}}
    "change_language" -> exit $ SelectLang newState
    _ -> continue newState

eval (ChangeVehicleAC (PopUpModal.OnButton2Click)) state = continue state {props {confirmChangeVehicle= false}}

eval (ChangeVehicleAC (PopUpModal.OnButton1Click)) state = exit $ ChangeVehicle state

eval (ChangeVehicleAC (PopUpModal.DismissPopup)) state = continue state {props {confirmChangeVehicle= false}}

eval _ state = continue state
