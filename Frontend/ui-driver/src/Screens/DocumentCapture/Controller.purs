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
import Components.PrimaryEditText as PrimaryEditText
import Components.MobileNumberEditor as MobileNumberEditor
import Prelude
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
import Data.Maybe (Maybe(..), isNothing, fromMaybe)
import Components.OptionsMenu as OptionsMenu
import Services.Config as SC
import Components.BottomDrawerList as BottomDrawerList
import Screens.Types as ST
import Helpers.Utils as HU
import Effect.Unsafe (unsafePerformEffect)
import Storage (KeyStore(..), getValueToLocalStore)
import Mobility.Prelude
import Common.Types.App (MobileNumberValidatorResp(..)) as MVR
import ConfigProvider
import Engineering.Helpers.Utils (mobileNumberValidator, mobileNumberMaxLength)
import Debug

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
            | BottomDrawerListAC BottomDrawerList.Action
            | WhatsAppClick Boolean
            | SSNPEAC PrimaryEditText.Action
            | FirstNameEditText PrimaryEditText.Action
            | LastNameEditText PrimaryEditText.Action
            | MobileEditText PrimaryEditText.Action
            | KeyboardCallback String
            | MobileNumberEditText MobileNumberEditor.Action
            | CallBackOpenCamera
            | UpdateShouldGoBack
            | PreviewSampleImage String
            | EmailEditText PrimaryEditText.Action

data ScreenOutput = GoBack 
                  | UploadAPI DocumentCaptureScreenState
                  | LogoutAccount
                  | SelectLang DocumentCaptureScreenState
                  | ChangeVehicle DocumentCaptureScreenState
                  | UpdateSSN DocumentCaptureScreenState
                  | UpdateProfile DocumentCaptureScreenState
                  | OpenCamera DocumentCaptureScreenState

eval :: Action -> DocumentCaptureScreenState -> Eval Action ScreenOutput DocumentCaptureScreenState

eval CallBackOpenCamera state = updateAndExit state $ OpenCamera state

eval (PrimaryButtonAC PrimaryButtonController.OnClick) state = 
  if state.props.isSSNView 
    then updateAndExit state $ UpdateSSN state
    else if state.props.isProfileView
      then
        case state.data.firstName, state.data.mobileNumber, state.data.email of
          Nothing, _, _ -> do
            _ <- pure $ JB.toggleBtnLoader "" false
            continue state{props{isValidFirstName = false}}
          Just name, Nothing, Just email -> do
            let isValidFirstName = (DS.length name) > 2 
                isValidEmail = JB.validateEmail email
                isValid = isValidFirstName && isValidEmail
                newState = state{props{isValidEmail = isValidEmail, isValidFirstName = isValidFirstName}}
            if isValid
              then updateAndExit state $ UpdateProfile newState 
              else do
                _ <- pure $ JB.toggleBtnLoader "" false
                continue newState
          Just name, Just mobileNumber, Nothing -> do 
            let config = getAppConfig appConfig
                validatorResp = mobileNumberValidator config.defaultCountryCodeConfig.countryCode config.defaultCountryCodeConfig.countryShortCode mobileNumber
                isValidFirstName = (DS.length name) > 2 
                isValid = isValidFirstName && (validatorResp == MVR.Valid)
            let newState = state{props{isValidMobileNumber = validatorResp == MVR.Valid, isValidFirstName = isValidFirstName}}
            if isValid 
              then updateAndExit state $ UpdateProfile newState 
              else do
                _ <- pure $ JB.toggleBtnLoader "" false
                continue newState
          _, _, _ -> continue state
      else
        continueWithCmd state [do
          JB.uploadFile (state.data.docType == ST.PROFILE_PHOTO)
          pure $ UpdateShouldGoBack ]

eval UpdateShouldGoBack state = continue state {props { shouldGoBack = state.data.docType /= ST.PROFILE_PHOTO }}

eval (PrimaryButtonAC PrimaryButtonController.NoAction) state = continue state

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

eval (ValidateDocumentModalAction (ValidateDocumentModal.PrimaryButtonActionController (PrimaryButtonController.NoAction))) state = continue state

eval (PopUpModalLogoutAction (PopUpModal.OnButton2Click)) state = continue $ (state {props {logoutModalView= false}})

eval (PopUpModalLogoutAction (PopUpModal.OnButton1Click)) state = exit $ LogoutAccount

eval (ValidateDocumentModalAction (ValidateDocumentModal.AfterRender)) state = continueWithCmd state [pure (CallBackImageUpload state.data.imageBase64 "" "")]

eval BackPressed state = 
  if state.props.validateDocModal then continue state { props { validateDocModal = false}}
  else if state.props.logoutModalView then continue state { props { logoutModalView = false}}
  else if state.props.confirmChangeVehicle then continue state{props{confirmChangeVehicle = false}}
  else if state.props.menuOptions then continue state{props{menuOptions = false}} 
  else if state.props.contactSupportModal == ST.SHOW then continue state { props { contactSupportModal = ST.ANIMATING}}
  else if state.props.previewSampleImage then continue state { props {previewSampleImage = false}}
  else if not state.props.shouldGoBack then continue state {props { shouldGoBack = true}}
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
    "whatsapp" -> continueWithCmd state [pure $ WhatsAppClick false]
    "email" -> continueWithCmd state [pure $ WhatsAppClick true]
    "call" -> do
                void $ pure $ unsafePerformEffect $ HU.contactSupportNumber ""
                continue state
    _ -> continue state

eval (PreviewSampleImage imgUrl) state = continue state {props {previewSampleImage = true, previewImgUrl = imgUrl}}

eval (WhatsAppClick isMail) state = continueWithCmd state [do
  let supportPhone = state.data.cityConfig.registration.supportWAN
      phone = "%0APhone%20Number%3A%20"<> getValueToLocalStore MOBILE_NUMBER_KEY
      dlNumber = getValueToLocalStore ENTERED_DL
      rcNumber = getValueToLocalStore ENTERED_RC
      dl = if (dlNumber /= "__failed") then ("%0ADL%20Number%3A%20"<> dlNumber) else ""
      rc = if (rcNumber /= "__failed") then ("%0ARC%20Number%3A%20"<> rcNumber) else ""
      url = if isMail then "mailto:" <> state.data.cityConfig.supportMail else "https://wa.me/" <> supportPhone <> "?text=Hi%20Team%2C%0AI%20would%20require%20help%20in%20onboarding%20%0A%E0%A4%AE%E0%A5%81%E0%A4%9D%E0%A5%87%20%E0%A4%AA%E0%A4%82%E0%A4%9C%E0%A5%80%E0%A4%95%E0%A4%B0%E0%A4%A3%20%E0%A4%AE%E0%A5%87%E0%A4%82%20%E0%A4%B8%E0%A4%B9%E0%A4%BE%E0%A4%AF%E0%A4%A4%E0%A4%BE%20%E0%A4%95%E0%A5%80%20%E0%A4%86%E0%A4%B5%E0%A4%B6%E0%A5%8D%E0%A4%AF%E0%A4%95%E0%A4%A4%E0%A4%BE%20%E0%A4%B9%E0%A5%8B%E0%A4%97%E0%A5%80" <> phone <> dl <> rc
  void $ if isMail then JB.openUrlInMailApp url else JB.openUrlInApp $ url
  pure NoAction
  ]

eval (SSNPEAC (PrimaryEditText.TextChanged id val)) state = do
  if (DS.length val) == 9 then 
      void $ pure $ JB.hideKeyboardOnNavigation true 
      else pure unit
  continue state {data{ssn = DS.trim val}}
eval (FirstNameEditText (PrimaryEditText.TextChanged id val)) state = do
  continue state{data{firstName = strToMaybe (DS.trim val)}, props{isValidFirstName = true, setDefault = false}}

eval (LastNameEditText (PrimaryEditText.TextChanged id val)) state = do
  continue state{data{lastName = strToMaybe (DS.trim val)}, props{setDefault = false}}

eval (MobileEditText (PrimaryEditText.TextChanged id newVal)) state = do
  let config = getAppConfig appConfig
  _ <- if DS.length newVal ==  mobileNumberMaxLength config.defaultCountryCodeConfig.countryShortCode then do
            pure $ JB.hideKeyboardOnNavigation true
            else pure unit
  let validatorResp = mobileNumberValidator config.defaultCountryCodeConfig.countryCode config.defaultCountryCodeConfig.countryShortCode newVal
  continue  state { props = state.props { isValidMobileNumber = isValidMobileNumber validatorResp }
                                        , data = state.data { mobileNumber = if validatorResp == MVR.MaxLengthExceeded then state.data.mobileNumber else strToMaybe newVal}}

eval (EmailEditText (PrimaryEditText.TextChanged id newVal)) state = do
  let config = getAppConfig appConfig
      validatorResp = mobileNumberValidator config.defaultCountryCodeConfig.countryCode config.defaultCountryCodeConfig.countryShortCode newVal
      email = strToMaybe (DS.trim newVal)
  continue  state { data { email = email}, props {isValidEmail = JB.validateEmail $ fromMaybe "" email}}

eval (KeyboardCallback event) state
  | event == "onKeyboardOpen" && (EHC.os == "IOS") =
    if state.props.isProfileView then do
      continueWithCmd state
        [ do 
          void $ JB.scrollToEnd (EHC.getNewIDWithTag "DocumentCaptureScrollView") true
          pure NoAction
        ]
    else
      update state
  | otherwise = update state

eval (MobileNumberEditText (MobileNumberEditor.TextChanged valId newVal)) state = do
  let config = getAppConfig appConfig
  _ <- if DS.length newVal ==  mobileNumberMaxLength config.defaultCountryCodeConfig.countryShortCode then do
            pure $ JB.hideKeyboardOnNavigation true
            else pure unit
  let validatorResp = mobileNumberValidator config.defaultCountryCodeConfig.countryCode config.defaultCountryCodeConfig.countryShortCode newVal
  continue  state { props = state.props { isValidMobileNumber = not (isValidMobileNumber validatorResp) }
                                        , data = state.data { mobileNumber = if validatorResp == MVR.MaxLengthExceeded then state.data.mobileNumber else strToMaybe newVal}}

eval _ state = update state


isValidMobileNumber :: MVR.MobileNumberValidatorResp -> Boolean 
isValidMobileNumber resp = (resp == MVR.ValidPrefix || resp == MVR.Valid)