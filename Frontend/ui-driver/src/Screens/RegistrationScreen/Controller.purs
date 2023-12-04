{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RegistrationScreen.Controller where

import Common.Types.App (LazyCheck(..))
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButtonController
import Components.GenericHeader as GenericHeader
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import Helpers.Utils (getStatus, contactSupportNumber)
import JBridge as JB
import Log (trackAppActionClick, trackAppBackPress, trackAppEndScreen, trackAppScreenEvent, trackAppScreenRender, trackAppTextInput)
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Prelude (class Show, bind, discard, pure, show, unit, ($), void)
import PrestoDOM (Eval, continue, continueWithCmd, exit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (RegisterationStep(..), RegistrationScreenState, StageStatus(..))
import Services.Config (getSupportNumber, getWhatsAppSupportNo)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
   performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen REGISTRATION_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen REGISTRATION_SCREEN)
      trackAppEndScreen appId (getScreen REGISTRATION_SCREEN)
    NoAction -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "in_screen" "no_action"
    AppOnboardingNavBarAC act -> case act of
      AppOnboardingNavBar.GenericHeaderAC genericHeaderAction -> case genericHeaderAction of 
        GenericHeader.PrefixImgOnClick -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "in_screen" "generic_header_on_click"
        GenericHeader.SuffixImgOnClick -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "in_screen" "generic_header_on_click"
      AppOnboardingNavBar.Logout -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "in_screen" "onboarding_nav_bar_logout"
      AppOnboardingNavBar.PrefixImgOnClick -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "in_screen" "app_onboarding_nav_bar_prefix_img_on_click"
    RegistrationAction value -> trackAppScreenRender appId "screen" (getScreen REGISTRATION_SCREEN)
    PopUpModalLogoutAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal" "on_goback"
      PopUpModal.Tipbtnclick _ _ -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal" "tip_button_click"
      PopUpModal.DismissPopup -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal" "dismiss_popup"
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal" "call_support"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal_action" "no_action"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal_action" "image"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen REGISTRATION_SCREEN) "popup_modal_action" "primary_edit_text"
      PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen REGISTRATION_SCREEN) "popup_modal_action" "countdown_updated"
      _ -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal_action" "no_action"
    PrimaryButtonAction act -> case act of 
      PrimaryButtonController.OnClick -> do
        trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "primary_button" "next_on_click"
        trackAppEndScreen appId (getScreen REGISTRATION_SCREEN)
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "primary_button" "no_action"
    _ -> trackAppActionClick appId (getScreen REGISTRATION_SCREEN) "popup_modal_action" "no_action"

    
data ScreenOutput = GoBack 
                  | GoToUploadDriverLicense RegistrationScreenState 
                  | GoToUploadVehicleRegistration RegistrationScreenState
                  | GoToPermissionScreen RegistrationScreenState
                  | LogoutAccount
                  | GoToOnboardSubscription
                  | GoToHomeScreen
                  | RefreshPage

data Action = BackPressed 
            | NoAction
            | AfterRender
            | RegistrationAction RegisterationStep
            | PopUpModalLogoutAction PopUpModal.Action
            | PrimaryButtonAction PrimaryButtonController.Action
            | Refresh
            | ContactSupport
            | AppOnboardingNavBarAC AppOnboardingNavBar.Action

eval :: Action -> RegistrationScreenState -> Eval Action ScreenOutput RegistrationScreenState
eval AfterRender state = continue state
eval BackPressed state = do
  void $ pure $ JB.minimizeApp ""
  continue state
eval (RegistrationAction item ) state = 
       case item of 
          DRIVING_LICENSE_OPTION -> exit $ GoToUploadDriverLicense state
          VEHICLE_DETAILS_OPTION -> exit $ GoToUploadVehicleRegistration state
          GRANT_PERMISSION -> exit $ GoToPermissionScreen state
          SUBSCRIPTION_PLAN -> exit GoToOnboardSubscription

eval (PopUpModalLogoutAction (PopUpModal.OnButton2Click)) state = continue $ (state {props {logoutModalView= false}})

eval (PopUpModalLogoutAction (PopUpModal.OnButton1Click)) state = exit $ LogoutAccount

eval (PopUpModalLogoutAction (PopUpModal.DismissPopup)) state = continue state {props {logoutModalView= false}}

eval (PrimaryButtonAction (PrimaryButtonController.OnClick)) state = exit GoToHomeScreen

eval Refresh state = exit RefreshPage

eval (AppOnboardingNavBarAC (AppOnboardingNavBar.Logout)) state = continue $ (state {props{logoutModalView = true}})

eval ContactSupport state = continueWithCmd state [do
  let merchant = getMerchant FunctionCall
  _ <- case merchant of
    NAMMAYATRI -> contactSupportNumber "WHATSAPP" 
    YATRISATHI -> JB.openWhatsAppSupport $ getWhatsAppSupportNo $ show merchant
    _ -> pure $ JB.showDialer (getSupportNumber "") false
  pure NoAction
  ]

eval _ state = continue state

getStatusValue :: String -> StageStatus
getStatusValue value = case value of
  "VALID" -> COMPLETED
  "PENDING" -> IN_PROGRESS
  "FAILED" -> FAILED
  "NO_DOC_AVAILABLE" -> NOT_STARTED
  "INVALID" -> FAILED
  "LIMIT_EXCEED" -> FAILED
  _ -> NOT_STARTED