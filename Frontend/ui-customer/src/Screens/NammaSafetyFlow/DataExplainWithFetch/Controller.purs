{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.DataExplainWithFetch.Controller where

import Prelude (class Show, pure, ($), show, not, map, (<>), (==), (+), (>=), (&&), (||), (/=), (<), (>), (-), discard, void, unit)
import PrestoDOM (Eval, update, defaultPerformLog, exit)
import PrestoDOM.Types.Core (class Loggable)
import PrestoDOM.Core (processEvent)
import Screens.Types (DataFetchScreenState, SafetyStageConfig, NammaSafetyStage(..), NewContacts, Component)
import Components.GenericHeader.Controller (Action(..)) as GenericHeaderController
import PrestoDOM (Eval, update, continue, exit, continueWithCmd, updateAndExit)
import Components.BannerCarousel as BannerCarousel
import RemoteConfig as RC
import Data.String as DS
import Effect.Unsafe (unsafePerformEffect)
import Locale.Utils (getLanguageLocale)
import Constants (languageKey)
import SessionCache (getValueFromWindow)
import Data.Array as Array
import Data.Int as DI
import Data.Maybe (Maybe(..), fromMaybe)
import PrestoDOM.List (ListItem)
import Components.BoxContainer as BoxContainer
import Components.DropDownWithHeader.Controller as DropDownWithHeader
import Components.PrimaryButton as PrimaryButton
import Services.API as API

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

data ScreenOutput
  = Exit DataFetchScreenState
  | AddEmergencyContacts DataFetchScreenState
  | UpdateEmergencyContacts DataFetchScreenState
  | GoToSafetyDrill DataFetchScreenState

data Action
  = AfterRender
  | GenericHeaderActionController GenericHeaderController.Action
  | BackPressed
  | PrimaryButtonAC PrimaryButton.Action
  | BoxContainerAC BoxContainer.Action
  | DropDownWithHeaderAC DropDownWithHeader.Action
  | DefaultContactSelected NewContacts

eval :: Action -> DataFetchScreenState -> Eval Action ScreenOutput DataFetchScreenState
eval AfterRender state = continue state

eval (GenericHeaderActionController (GenericHeaderController.PrefixImgOnClick)) state = continueWithCmd state [ do pure BackPressed ]

eval (BoxContainerAC (BoxContainer.OnClick action)) state = case action of
  "UpdateUnExpectedEventChecks" -> continue state { data { unExpectedEventChecks = (toggleRideShareOptions state.data.unExpectedEventChecks) } }
  "PostRideCheck" -> continue state { data { postRideCheck = toggleRideShareOptions state.data.postRideCheck } }
  "NotifySafetyTeam" -> continue state { data { notifySafetyTeam = not state.data.notifySafetyTeam } }
  "EmergencySOSShake" -> continue state { data { emergencySOSShake = not state.data.emergencySOSShake } }
  "AutomaticCallOnEmergency" -> continue state { data { autoCallDefaultContact = not state.data.autoCallDefaultContact } }
  _ -> continue state

eval BackPressed state = if state.config.currentStep > 0 then continue state { config { currentStep = state.config.currentStep - 1 } } else exit $ Exit state

eval (DefaultContactSelected contact) state =
  let
    updatedContactList = map (\ct -> if ct.number == contact.number then ct { priority = 0 } else ct { priority = 1 }) state.data.emergencyContactsList
  in
    continue state { data { defaultSelectedContact = contact, emergencyContactsList = updatedContactList } }

eval (DropDownWithHeaderAC (DropDownWithHeader.OnExpand contact action)) state = continue state { props { dropDownAction = action, showDropDown = not state.props.showDropDown } }

eval (DropDownWithHeaderAC (DropDownWithHeader.OnSelect option action)) state = case action of
  "UpdateUnExpectedEventChecks" -> continue state { data { unExpectedEventChecks = option.key }, props { showDropDown = not state.props.showDropDown } }
  "PostRideCheck" -> continue state { data { postRideCheck = option.key }, props { showDropDown = not state.props.showDropDown } }
  _ -> continue state

eval (PrimaryButtonAC PrimaryButton.OnClick) state =
  let
    index = state.config.currentStep + 1

    currentStageConfig = getStepConfig state
  in
    if state.props.stageSetUpCompleted && currentStageConfig.primaryButtonAction /= "SafetyTestDrill" then
      exit $ Exit state
    else case currentStageConfig.primaryButtonAction of
      "AddContactsFlow" -> updateAndExit state $ AddEmergencyContacts state
      "UpdateEmergencyContacts" -> updateAndExit state $ UpdateEmergencyContacts state
      "SafetyTestDrill" -> updateAndExit state $ GoToSafetyDrill state
      _ ->
        if index < state.config.stageSteps then
          continue state { config { currentStep = index } }
        else
          exit $ Exit state

eval _ state = continue state

toggleRideShareOptions :: API.RideShareOptions -> API.RideShareOptions
toggleRideShareOptions options = case options of
  API.NEVER_SHARE -> API.ALWAYS_SHARE
  API.SHARE_WITH_TIME_CONSTRAINTS -> API.NEVER_SHARE
  API.ALWAYS_SHARE -> API.NEVER_SHARE

getStageConfig :: DataFetchScreenState -> Array SafetyStageConfig
getStageConfig state = case state.config.stage of
  TrustedContacts items -> items
  SafetyCheckIn items -> items
  EmergencyActions items -> items
  SafetyDrill items -> items
  DriverSafetyStandards items -> items
  TrustedContactsActions items -> items

getStepConfig :: DataFetchScreenState -> SafetyStageConfig
getStepConfig state =
  let
    index = state.config.currentStep
  in
    case state.config.stage of
      TrustedContacts items -> fromMaybe emptyStageConfig $ items Array.!! index
      SafetyCheckIn items -> fromMaybe emptyStageConfig $ items Array.!! index
      EmergencyActions items -> fromMaybe emptyStageConfig $ items Array.!! index
      SafetyDrill items -> fromMaybe emptyStageConfig $ items Array.!! index
      DriverSafetyStandards items -> fromMaybe emptyStageConfig $ items Array.!! index
      TrustedContactsActions items -> fromMaybe emptyStageConfig $ items Array.!! index

emptyStageConfig :: SafetyStageConfig
emptyStageConfig =
  { dynamicViewData: []
  , imageUrl: ""
  , primaryButtonAction: ""
  , primaryButtonText: ""
  }
