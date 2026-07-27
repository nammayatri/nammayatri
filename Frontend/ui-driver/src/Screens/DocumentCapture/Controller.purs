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
import Prelude (class Show, pure, unit, bind, discard, ($), (/=), (==), void, (<>), show)
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit)
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
import Components.BottomDrawerList as BottomDrawerList
import Screens.Types as ST
import Helpers.Utils as HU
import Effect.Unsafe (unsafePerformEffect)
import Storage (KeyStore(..), getValueToLocalStore)
import Common.Types.App
import Engineering.Helpers.Events as EHE

instance showAction :: Show Action where
  show (PrimaryButtonAC var1) = "PrimaryButtonAC_" <> show var1
  show (GenericHeaderAC var1) = "GenericHeaderAC_" <> show var1
  show (AppOnboardingNavBarAC var1) = "AppOnboardingNavBarAC_" <> show var1
  show (CallBackImageUpload _ _ _) = "CallBackImageUpload"
  show (ValidateDocumentModalAction var1) = "ValidateDocumentModalAction_" <> show var1
  show (PopUpModalLogoutAction var1) = "PopUpModalLogoutAction_" <> show var1
  show (OptionsMenuAction var1) = "OptionsMenuAction_" <> show var1
  show (BackPressed) = "BackPressed"
  show (NoAction) = "NoAction"
  show (ChangeVehicleAC var1) = "ChangeVehicleAC_" <> show var1
  show (BottomDrawerListAC var1) = "BottomDrawerListAC_" <> show var1
  show (WhatsAppClick) = "WhatsAppClick"

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
            | BottomDrawerListAC BottomDrawerList.Action
            | WhatsAppClick

data ScreenOutput = GoBack 
                  | UploadAPI DocumentCaptureScreenState
                  | LogoutAccount
                  | SelectLang DocumentCaptureScreenState
                  | ChangeVehicle DocumentCaptureScreenState

uploadFileConfig :: UploadFileConfig
uploadFileConfig = UploadFileConfig {
  showAccordingToAspectRatio : false,
  imageAspectHeight : 0,
  imageAspectWidth : 0
}

eval :: Action -> DocumentCaptureScreenState -> Eval Action ScreenOutput DocumentCaptureScreenState

eval (PrimaryButtonAC PrimaryButtonController.OnClick) state = continueWithCmd state [do
  let _ = EHE.addEvent (EHE.defaultEventObject $ HU.getDocUploadEventName state.data.docType) { module = HU.getRegisterationStepModule state.data.docType, source = HU.getRegisterationStepScreenSource state.data.docType}
  _ <- liftEffect $ JB.uploadFile uploadFileConfig true
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
    void $ liftEffect $ JB.uploadFile uploadFileConfig true
    pure NoAction]

eval (PopUpModalLogoutAction (PopUpModal.OnButton2Click)) state = continue $ (state {props {logoutModalView= false}})

eval (PopUpModalLogoutAction (PopUpModal.OnButton1Click)) state = exit $ LogoutAccount

eval (ValidateDocumentModalAction (ValidateDocumentModal.AfterRender)) state = continueWithCmd state [pure (CallBackImageUpload state.data.imageBase64 "" "")]

eval BackPressed state = 
  if state.props.validateDocModal then continue state { props { validateDocModal = false}}
  else if state.props.logoutModalView then continue state { props { logoutModalView = false}}
  else if state.props.confirmChangeVehicle then continue state{props{confirmChangeVehicle = false}}
  else if state.props.menuOptions then continue state{props{menuOptions = false}} 
  else if state.props.contactSupportModal == ST.SHOW then continue state { props { contactSupportModal = ST.ANIMATING}}
  else exit $ GoBack

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
    "call" -> do
                void $ pure $ unsafePerformEffect $ HU.contactSupportNumber ""
                continue state
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
