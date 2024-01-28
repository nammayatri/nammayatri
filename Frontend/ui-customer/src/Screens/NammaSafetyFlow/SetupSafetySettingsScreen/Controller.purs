{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.SetupSafetySettingsScreen.Controller where

import Prelude
import PrestoDOM
import Screens.Types
import Components.GenericHeader.Controller as GenericHeaderController
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton.Controller as PrimaryButtonController
import Screens.NammaSafetyFlow.Components.SafetyUtils (getDefaultPriorityList)
import Components.StepsHeaderModel.Controller as StepsHeaderModelController
import Constants (defaultDensity)
import Data.Array as DA
import Data.Function.Uncurried (runFn1)
import Data.Int as DI
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as DS
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (launchAff)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils (loaderText, toggleLoader)
import Helpers.Utils as HU
import JBridge (askRequestedPermissions, openUrlInApp, showDialer, stopRecord, getLayoutBounds)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog, trackAppActionClick, trackAppBackPress, trackAppEndScreen, trackAppScreenRender)
import Presto.Core.Types.Language.Flow (delay)
import PrestoDOM.Core (getPushFn)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.NammaSafetyFlow.Components.ContactsList as ContactList
import Services.API (ContactDetails(..), GetEmergencySettingsRes(..))
import Services.Config (getSupportNumber)
import Storage (KeyStore(..), getValueToLocalStore, setValueToLocalStore)
import Types.App (defaultGlobalState)
import Types.EndPoint (updateSosVideo)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    StepsHeaderModelAC _ -> trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "steps_header_modal" "backpressed"
    BackPressed -> trackAppBackPress appId (getScreen NAMMASAFETY_SCREEN)
    GoToNextStep act -> case act of
      PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "next_step_onboard" "primary button"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "no_action" "primary button"
    _ -> trackAppScreenRender appId "screen" (getScreen NAMMASAFETY_SCREEN)

data ScreenOutput
  = GoBack NammaSafetyScreenState
  | PostContacts NammaSafetyScreenState
  | Refresh NammaSafetyScreenState
  | PostEmergencySettings NammaSafetyScreenState
  | GoToEmergencyContactScreen NammaSafetyScreenState

data Action
  = BackPressed
  | NoAction
  | StepsHeaderModelAC StepsHeaderModelController.Action
  | GoToNextStep PrimaryButtonController.Action
  | EditEmergencyContacts PrimaryButtonController.Action
  | SwitchToStage SafetySetupStage
  | ToggleSwitch SafetySetupStage
  | PopUpModalAction PopUpModal.Action
  | AddContacts
  | UpdateEmergencySettings GetEmergencySettingsRes
  | DisableShimmer
  | ContactListAction ContactList.Action

eval :: Action -> NammaSafetyScreenState -> Eval Action ScreenOutput NammaSafetyScreenState
eval AddContacts state = updateAndExit state $ GoToEmergencyContactScreen state

eval (StepsHeaderModelAC StepsHeaderModelController.OnArrowClick) state = continueWithCmd state [ pure BackPressed ]

eval (ToggleSwitch stage) state = case stage of
  SetNightTimeSafetyAlert -> continue state { data { nightSafetyChecks = not state.data.nightSafetyChecks } }
  SetDefaultEmergencyContacts ->
    if DA.length state.data.contactsList /= 0 then
      continue state { data { shareToEmergencyContacts = not state.data.shareToEmergencyContacts } }
    else
      continueWithCmd state [ pure AddContacts ]
  _ -> continue state

eval (EditEmergencyContacts PrimaryButtonController.OnClick) state = updateAndExit state $ GoToEmergencyContactScreen state

eval (GoToNextStep PrimaryButtonController.OnClick) state = do
  case state.props.setupStage of
    SetNightTimeSafetyAlert -> continue state { props { setupStage = SetPersonalSafetySettings } }
    SetDefaultEmergencyContacts -> continue state { props { setupStage = SetNightTimeSafetyAlert } }
    SetPersonalSafetySettings ->
      do
        void $ pure $ askRequestedPermissions [ "android.permission.CALL_PHONE" ]
        exit $ PostEmergencySettings state
    _ -> continue state

eval (PopUpModalAction PopUpModal.OnButton2Click) state = do
  let
    newContacts = DA.filter (\x -> x.number <> x.name /= state.data.removedContactDetail.number <> state.data.removedContactDetail.name) state.data.contactsList
  contactsInString <- pure $ HU.toStringJSON newContacts
  void $ pure $ setValueToLocalStore CONTACTS contactsInString
  exit $ PostContacts state { data { contactsList = newContacts } }

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue state { props { showInfoPopUp = false } }

eval (BackPressed) state = do
  if state.props.confirmPopup then
    continue state { props { confirmPopup = false } }
  else case state.props.setupStage of
    SetNightTimeSafetyAlert -> continue state { props { setupStage = SetDefaultEmergencyContacts } }
    SetDefaultEmergencyContacts -> exit $ GoBack state
    SetPersonalSafetySettings -> continue state { props { setupStage = SetNightTimeSafetyAlert } }
    _ -> continue state

eval DisableShimmer state = continue state { props { showShimmer = false } }

eval (ContactListAction (ContactList.RemoveButtonClicked contactDetail)) state = continue state { props { showInfoPopUp = true }, data { removedContactDetail = contactDetail } }

eval (ContactListAction (ContactList.ContactCardClicked index)) state = do
  let
    newContactsList =
      DA.mapWithIndex
        ( \i contact ->
            if i == index then
              contact { priority = 0 }
            else
              contact { priority = 1 }
        )
        state.data.contactsList
  continue state { data { contactsList = newContactsList } }

eval (ContactListAction ContactList.AddContacts) state = do
  exit $ GoToEmergencyContactScreen state

eval (_) state = continue state
