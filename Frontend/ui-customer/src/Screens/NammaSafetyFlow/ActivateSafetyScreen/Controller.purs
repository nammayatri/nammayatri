{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.ActivateSafetyScreen.Controller where

import JBridge as JB
import Log
import Prelude (class Show, bind, discard, map, not, pure, show, void, ($), (&&), (/=), (==))
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import Screens.Types (NammaSafetyScreenState)
import Timers (clearTimerWithId)
import Common.Types.App as CTA
import Components.GenericHeader.Controller as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Data.Array as DA
import Data.Maybe (Maybe(..), fromMaybe)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Screens.NammaSafetyFlow.Components.HeaderView as Header
import Screens.NammaSafetyFlow.Components.SafetyUtils (getDefaultPriorityList)
import Screens.NammaSafetyFlow.ScreenData (defaultTimerValue)
import Services.API (ContactDetails(..), GetEmergencySettingsRes(..), Sos(..), SosFlow(..), RideShareOptions(..))
import Services.Config (getSupportNumber)
import PrestoDOM.Types.Core (class Loggable, defaultPerformLog)
import Components.SourceToDestination as SourceToDestination

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

data ScreenOutput
  = GoBack NammaSafetyScreenState
  | GoToEmergencyContactScreen NammaSafetyScreenState
  | GoToSosScreen NammaSafetyScreenState
  | CreateSos NammaSafetyScreenState Boolean
  | GoToEducationScreen NammaSafetyScreenState
  | GoToIssueScreen NammaSafetyScreenState
  | NotifyMockDrill NammaSafetyScreenState

data Action
  = BackPressed
  | NoAction
  | SafetyHeaderAction Header.Action
  | CancelSosTrigger PrimaryButtonController.Action
  | AddContacts
  | UpdateEmergencySettings GetEmergencySettingsRes
  | DisableShimmer
  | CountDown Int String String
  | TriggerSosCountdown
  | StartTestDrill PrimaryButtonController.Action
  | GoToSafetySettings
  | ContactAction ContactCircle.Action
  | GoToTestDrill
  | UpdateSosId Sos
  | GoToActiveSos
  | CallPolice
  | ShowPoliceView
  | ShowSafetyIssueView
  | SelectedCurrentLocation Number Number String
  | GoToEducationView
  | CallSupport
  | SourceToDestinationAC SourceToDestination.Action
  | AlertSafetyTeam

eval :: Action -> NammaSafetyScreenState -> Eval Action ScreenOutput NammaSafetyScreenState
eval AddContacts state = updateAndExit state $ GoToEmergencyContactScreen state

eval (UpdateEmergencySettings (GetEmergencySettingsRes response)) state = do
  let
    contacts =
      map
        ( \(ContactDetails item) ->
            { number: item.mobileNumber
            , name: item.name
            , isSelected: true
            , enableForFollowing: fromMaybe false item.enableForFollowing
            , enableForShareRide: fromMaybe false item.enableForShareRide
            , onRide : fromMaybe false item.onRide
            , priority: fromMaybe 1 item.priority
            }
        )
        response.defaultEmergencyNumbers
  continue
    state
      { data
        { hasCompletedSafetySetup = response.hasCompletedSafetySetup
        , shareToEmergencyContacts = response.shareEmergencyContacts
        , nightSafetyChecks = response.nightSafetyChecks
        , hasCompletedMockSafetyDrill = response.hasCompletedMockSafetyDrill
        , shareTripWithEmergencyContactOption = shareTripOption response.shareTripWithEmergencyContactOption
        , shareOptionCurrent = shareTripOption response.shareTripWithEmergencyContactOption
        , emergencyContactsList = getDefaultPriorityList contacts
        }
      , props { enableLocalPoliceSupport = response.enablePoliceSupport, localPoliceNumber = fromMaybe "" response.localPoliceNumber }
      }
  where
  shareTripOption val = case val of -- Handling Backward compatibility
    Just option -> option
    Nothing -> case response.shareTripWithEmergencyContacts of
      Just shareTrip ->
        if shareTrip then
          SHARE_WITH_TIME_CONSTRAINTS
        else
          NEVER_SHARE
      Nothing -> NEVER_SHARE

eval (SafetyHeaderAction (Header.GenericHeaderAC GenericHeaderController.PrefixImgOnClick)) state = continueWithCmd state [ pure BackPressed ]

eval (SafetyHeaderAction (Header.BackClicked)) state = continueWithCmd state [ pure BackPressed ]

eval (SafetyHeaderAction (Header.LearnMoreClicked)) state = do
  _ <- pure $ clearTimerWithId state.props.timerId
  exit $ GoToEducationScreen state { props { triggeringSos = false, timerValue = defaultTimerValue, timerId = "", confirmTestDrill = false } }

eval (CancelSosTrigger PrimaryButtonController.OnClick) state = do
  _ <- pure $ clearTimerWithId state.props.timerId
  exit $ GoBack state { props { triggeringSos = false, timerValue = defaultTimerValue, timerId = "" } }

eval BackPressed state =
  if state.props.showCallPolice then
    continue state { props { showCallPolice = false } }
  else 
    exit $ GoBack state

eval DisableShimmer state = continue state { props { showShimmer = false } }

eval (StartTestDrill PrimaryButtonController.OnClick) state =
  exit $ NotifyMockDrill
    state
      { props
        { confirmTestDrill = false
        , showTestDrill = true
        , timerValue = defaultTimerValue
        }
      }

eval (CountDown seconds status timerID) state = do
  _ <- pure $ printLog "timer" $ show seconds
  if status == "EXPIRED" then do
    _ <- pure $ clearTimerWithId state.props.timerId
    let
      newState = state { props { timerId = "" } }
    updateAndExit newState $ CreateSos newState false
  else
    continue $ state { props { timerValue = seconds, timerId = timerID } }

eval TriggerSosCountdown state = continue state { props { triggeringSos = true } }

eval AlertSafetyTeam state = exit $ CreateSos state false

eval (ContactAction (ContactCircle.OnClick index)) state = do
  let
    newContacts =
      DA.mapWithIndex
        ( \i contact ->
            if i == index then
              contact { priority = 0 }
            else
              contact { priority = 1 }
        )
        state.data.emergencyContactsList
  continue state { data { emergencyContactsList = newContacts } }

eval GoToTestDrill state = do
  _ <- pure $ clearTimerWithId state.props.timerId
  continue
    state
      { props
        { confirmTestDrill = true
        , showTestDrill = false
        , timerValue = defaultTimerValue
        , triggeringSos = false
        }
      }

eval GoToActiveSos state = 
  exit $ GoToSosScreen state { props { triggeringSos = false, timerValue = defaultTimerValue, timerId = "" } }

eval (UpdateSosId (Sos sos)) state = do
  if sos.flow /= Police && DA.notElem sos.status [CTA.Resolved, CTA.MockResolved] then do
    let
      newState = state { data { sosId = sos.id, sosType = Just sos.flow } }

    if sos.status == CTA.MockPending && state.props.confirmTestDrill then do
      exit $ GoToSosScreen newState{props{showTestDrill = true, confirmTestDrill = false}}
    else if sos.status == CTA.MockPending && not state.props.confirmTestDrill then do
      continue newState
    else
      exit $ GoToSosScreen newState
  else do
    continue state

eval ShowPoliceView state = continue state { props { showCallPolice = true } }

eval CallPolice state = do
  void $ pure $ JB.showDialer "112" false
  exit $ CreateSos state true

eval ShowSafetyIssueView state = exit $ GoToIssueScreen state

eval (SelectedCurrentLocation _ _ name) state = continue state { data { currentLocation = name } }

eval GoToEducationView state = do
  _ <- pure $ clearTimerWithId state.props.timerId
  exit $ GoToEducationScreen state { props { triggeringSos = false, timerValue = defaultTimerValue, timerId = "" } }

eval CallSupport state = do
  void $ pure $ JB.showDialer (getSupportNumber "") false
  continue state

eval _ state = continue state
