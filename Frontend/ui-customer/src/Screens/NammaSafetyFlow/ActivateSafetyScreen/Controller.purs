{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.ActivateSafetyScreen.Controller
  ( Action(..)
  , ScreenOutput(..)
  , eval
  ) where

import Log
import Prelude
import PrestoDOM
import Screens.Types
import Storage
import Timers

import Components.GenericHeader.Controller as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Data.Array as DA
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as DS
import Engineering.Helpers.Commons as EHC
import Helpers.Utils as HU
import JBridge
import Debug
import Language.Strings (getString)
import Language.Types (STR(..))
import Presto.Core.Types.Language.Flow (delay)
import PrestoDOM.Core (getPushFn)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Screens.NammaSafetyFlow.Components.HeaderView as Header
import Screens.NammaSafetyFlow.Components.SafetyUtils (getDefaultPriorityList)
import Services.API (ContactDetails(..), GetEmergencySettingsRes(..))
import Services.Config (getSupportNumber)
import Types.App (defaultGlobalState)
import Types.EndPoint (updateSosVideo)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    BackPressed -> trackAppBackPress appId (getScreen NAMMASAFETY_SCREEN)
    _ -> trackAppScreenRender appId "screen" (getScreen NAMMASAFETY_SCREEN)

data ScreenOutput
  = GoBack NammaSafetyScreenState
  | GoToEmergencyContactScreen NammaSafetyScreenState
  | GoToSosScreen NammaSafetyScreenState
  | CreateSos NammaSafetyScreenState Boolean
  | GoToEducationScreen NammaSafetyScreenState
  | GoToIssueScreen NammaSafetyScreenState

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
  | UpdateSosId String
  | GoToActiveSos
  | CallPolice
  | ShowPoliceView
  | ShowSafetyIssueView
  | SelectedCurrentLocation Number Number String
  | GoToEducationView

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
            , enableForFollowing: false
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
        , shareTripWithEmergencyContacts = response.shareTripWithEmergencyContacts
        , emergencyContactsList = getDefaultPriorityList contacts
        }
      , props { enableLocalPoliceSupport = response.enablePoliceSupport, localPoliceNumber = fromMaybe "" response.localPoliceNumber }
      }

eval (SafetyHeaderAction (Header.GenericHeaderAC GenericHeaderController.PrefixImgOnClick)) state = continueWithCmd state [ pure BackPressed ]

eval (SafetyHeaderAction (Header.LearnMoreClicked)) state = do
  _ <- pure $ clearTimerWithId state.props.timerId
  exit $ GoToEducationScreen state { props { triggeringSos = false, timerValue = 6, timerId = "", confirmTestDrill = false } }

eval (CancelSosTrigger PrimaryButtonController.OnClick) state = do
  _ <- pure $ clearTimerWithId state.props.timerId
  exit $ GoBack state { props { triggeringSos = false, timerValue = 6, timerId = "" } }

eval (BackPressed) state = if state.props.showCallPolice then continue state{props{showCallPolice = false}} 
  else exit $ GoBack state

eval DisableShimmer state = continue state { props { showShimmer = false } }

eval (StartTestDrill PrimaryButtonController.OnClick) state =
  continue
    state
      { props
        { confirmTestDrill = false
        , showTestDrill = true
        , timerValue = 6
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
        , timerValue = 6
        , triggeringSos = false
        }
      }

eval GoToActiveSos state = do
  exit $ GoToSosScreen state { props { triggeringSos = false, timerValue = 6, timerId = "" } }

eval (UpdateSosId sosId) state = do
  let
    newState = state { data { sosId = sosId } }
  exit $ GoToSosScreen newState

eval ShowPoliceView state = continue state{props{showCallPolice = true}}

eval CallPolice state = do
  void $ pure $ showDialer "112" false
  exit $ CreateSos state true

eval ShowSafetyIssueView state = exit $ GoToIssueScreen state

eval (SelectedCurrentLocation lat lon name) state = 
  continue state  { data { currentLocation =  name  } }

eval GoToEducationView state = exit $ GoToEducationScreen state

eval (_) state = continue state
