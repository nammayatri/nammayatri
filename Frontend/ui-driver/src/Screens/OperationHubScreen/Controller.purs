{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.OperationHubScreen.Controller where

import Data.Maybe
import Prelude
import PrestoDOM.Types.Core (class Loggable)
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import PrestoDOM (Eval, update, continue, continueWithCmd, exit)
import Screens.Types as ST
import Components.OptionsMenu as OptionsMenu
import Components.BottomDrawerList as BottomDrawerList
import Effect.Unsafe (unsafePerformEffect)
import Helpers.Utils as HU
import JBridge as JB
import Storage (KeyStore(..), getValueToLocalStore)
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Services.API as API
import Screens.OperationHubScreen.ScreenData

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    _ -> pure unit

data Action = NoAction
           | BackPressed
           | AppOnboardingNavBarAC AppOnboardingNavBar.Action
           | OptionsMenuAction OptionsMenu.Action
           | BottomDrawerListAC BottomDrawerList.Action
           | WhatsAppClick
           | PopUpModalLogoutAction PopUpModal.Action
           | PrimaryButtonAC PrimaryButton.Action
           | ShowOptions
           | HubSelected API.OperationHub
           | OpenMaps

data ScreenOutput = GoBack 
                    | SelectLang ST.OperationHubScreenState
                    | LogoutAccount
                    | DriverOperationCreateRequest ST.OperationHubScreenState
                    | GoToFaqsScreen ST.OperationHubScreenState

eval :: Action -> ST.OperationHubScreenState -> Eval Action ScreenOutput ST.OperationHubScreenState

eval NoAction state = continue state

eval BackPressed state = 
  if state.props.logoutModalView then continue state { props { logoutModalView = false}}
  else if state.props.menuOptions then continue state{props{menuOptions = false}} 
  else if state.props.contactSupportModal == ST.SHOW then continue state { props { contactSupportModal = ST.ANIMATING}}
  else exit $ GoBack

eval (AppOnboardingNavBarAC (AppOnboardingNavBar.Logout)) state = continue state {props { menuOptions = true }}

eval (AppOnboardingNavBarAC AppOnboardingNavBar.PrefixImgOnClick) state = continueWithCmd state [pure BackPressed]

eval (OptionsMenuAction OptionsMenu.BackgroundClick) state = continue state{props{menuOptions = false}}

eval (HubSelected hub) state = continue state { data { selectedHub = Just hub }, props { showOptions = false } }

eval (OptionsMenuAction (OptionsMenu.ItemClick item)) state = do
  let newState = state{props{menuOptions = false}}
  case item of
    "logout" -> continue newState {props { logoutModalView = true }}
    "contact_support" -> continue newState { props { contactSupportModal = ST.SHOW}}
    "change_language" -> exit $ SelectLang newState
    "faqs" -> exit $ GoToFaqsScreen newState
    _ -> continue newState

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
  let cityConfig = HU.getCityConfig state.data.config.cityConfig (getValueToLocalStore DRIVER_LOCATION)
      supportPhone = cityConfig.registration.supportWAN
      phone = "%0APhone%20Number%3A%20"<> getValueToLocalStore MOBILE_NUMBER_KEY
      dlNumber = getValueToLocalStore ENTERED_DL
      rcNumber = getValueToLocalStore ENTERED_RC
      dl = if (dlNumber /= "__failed") then ("%0ADL%20Number%3A%20"<> dlNumber) else ""
      rc = if (rcNumber /= "__failed") then ("%0ARC%20Number%3A%20"<> rcNumber) else ""
  void $ JB.openUrlInApp $ "https://wa.me/" <> supportPhone <> "?text=Hi%20Team%2C%0AI%20would%20require%20help%20in%20onboarding%20%0A%E0%A4%AE%E0%A5%81%E0%A4%9D%E0%A5%87%20%E0%A4%AA%E0%A4%82%E0%A4%9C%E0%A5%80%E0%A4%95%E0%A4%B0%E0%A4%A3%20%E0%A4%AE%E0%A5%87%E0%A4%82%20%E0%A4%B8%E0%A4%B9%E0%A4%BE%E0%A4%AF%E0%A4%A4%E0%A4%BE%20%E0%A4%95%E0%A5%80%20%E0%A4%86%E0%A4%B5%E0%A4%B6%E0%A5%8D%E0%A4%AF%E0%A4%95%E0%A4%A4%E0%A4%BE%20%E0%A4%B9%E0%A5%8B%E0%A4%97%E0%A5%80" <> phone <> dl <> rc
  pure NoAction
  ]

eval (PopUpModalLogoutAction (PopUpModal.OnButton2Click)) state = continue $ (state {props {logoutModalView= false}})

eval (PopUpModalLogoutAction (PopUpModal.OnButton1Click)) state = exit $ LogoutAccount

eval ShowOptions state = continue state {data {selectedHub = Nothing}, props { showOptions = not $ state.props.showOptions }}

eval OpenMaps state = do
  let (API.OperationHub hub) = fromMaybe dummyOperationHub state.data.selectedHub
  void $ pure $ JB.openNavigation hub.lat hub.lon "DRIVE"
  continue state

eval (PrimaryButtonAC PrimaryButton.OnClick) state = exit $ DriverOperationCreateRequest state

eval _ state = update state