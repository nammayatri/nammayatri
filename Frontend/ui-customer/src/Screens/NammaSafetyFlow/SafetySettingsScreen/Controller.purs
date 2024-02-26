{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.SafetySettingsScreen.Controller where

import JBridge as JB 
import Log (trackAppActionClick, trackAppBackPress, trackAppScreenRender)
import Prelude (class Show, discard, map, not, pure, void, ($), (/=), (==))
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import Screens.Types (NammaSafetyScreenState, SafetySetupStage(..))
import Components.GenericHeader.Controller as GenericHeaderController
import Components.PopupWithCheckbox.Controller as PopupWithCheckboxController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Data.Array as DA
import Data.Maybe (Maybe(..), fromMaybe)
import Foreign (unsafeToForeign)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.NammaSafetyFlow.ComponentConfig (labelData)
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Screens.NammaSafetyFlow.Components.HeaderView as Header
import Screens.NammaSafetyFlow.Components.SafetyUtils (getDefaultPriorityList)
import Services.API (ContactDetails(..), GetEmergencySettingsRes(..), RideShareOptions(..))

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    BackPressed -> trackAppBackPress appId (getScreen NAMMASAFETY_SCREEN)
    StartNammaSafetyOnboarding act -> case act of
      PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "start_onboarding" "primary button"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "no_action" "primary button"
    GoToNextStep act -> case act of
      PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "next_step_onboard" "primary button"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "no_action" "primary button"
    _ -> trackAppScreenRender appId "screen" (getScreen NAMMASAFETY_SCREEN)

data ScreenOutput
  = GoBack NammaSafetyScreenState
  | PostEmergencySettings NammaSafetyScreenState
  | GoToEmergencyContactScreen NammaSafetyScreenState
  | GoToEducationScreen NammaSafetyScreenState
  | GoToSetupScreen NammaSafetyScreenState
  | GoToActivateSosScreen NammaSafetyScreenState
  | PostContacts NammaSafetyScreenState

data Action
  = BackPressed
  | NoAction
  | SafetyHeaderAction Header.Action
  | StartNammaSafetyOnboarding PrimaryButtonController.Action
  | GoToNextStep PrimaryButtonController.Action
  | EditEmergencyContacts
  | SwitchToStage SafetySetupStage
  | ToggleSwitch SafetySetupStage
  | AddContacts
  | UpdateEmergencySettings GetEmergencySettingsRes
  | DisableShimmer
  | ChangeFollowing Int
  | GoToEducationView
  | StartTestDrill PrimaryButtonController.Action
  | ContactAction ContactCircle.Action
  | ShowShareTripOptions
  | ShareTripOptionPopup PopupWithCheckboxController.Action

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
            , priority: fromMaybe 1 item.priority
            , onRide : fromMaybe false item.onRide
            }
        )
        response.defaultEmergencyNumbers
  void $ pure $ JB.setCleverTapUserProp [ { key: "Safety Setup Completed", value: unsafeToForeign response.hasCompletedSafetySetup } ]
  void $ pure $ JB.setCleverTapUserProp [ { key: "Auto Share Night Ride", value: unsafeToForeign response.shareTripWithEmergencyContacts } ]
  void $ pure $ JB.setCleverTapUserProp [ { key: "Mock Safety Drill Completed", value: unsafeToForeign response.hasCompletedMockSafetyDrill } ]
  void $ pure $ JB.setCleverTapUserProp [ { key: "Night Safety Check Enabled", value: unsafeToForeign response.nightSafetyChecks } ]
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

eval (SafetyHeaderAction (Header.LearnMoreClicked)) state = exit $ GoToEducationScreen state

eval (ToggleSwitch stage) state = case stage of
  SetNightTimeSafetyAlert -> exit $ PostEmergencySettings state { data { nightSafetyChecks = not state.data.nightSafetyChecks } }
  SetDefaultEmergencyContacts ->
    if DA.length state.data.emergencyContactsList /= 0 then
      exit $ PostEmergencySettings state { data { shareToEmergencyContacts = not state.data.shareToEmergencyContacts } }
    else
      continueWithCmd state [ pure AddContacts ]
  _ -> continue state

eval (StartNammaSafetyOnboarding PrimaryButtonController.OnClick) state = exit $ GoToSetupScreen state

eval EditEmergencyContacts state = updateAndExit state $ GoToEmergencyContactScreen state

eval (SwitchToStage stage) state = continue state { props { setupStage = stage } }

eval (BackPressed) state = exit $ GoBack state

eval DisableShimmer state = continue state { props { showShimmer = false } }

eval (ChangeFollowing contactIndex) state = do
  let
    newContacts =
      DA.mapWithIndex
        ( \index item -> case index == contactIndex of
            true -> item { enableForFollowing = not item.enableForFollowing }
            false -> item
        )
        state.data.emergencyContactsList

    newState = state { data { emergencyContactsList = newContacts } }
  updateAndExit newState $ PostContacts newState

eval GoToEducationView state = exit $ GoToEducationScreen state

eval (StartTestDrill PrimaryButtonController.OnClick) state =
  exit
    $ GoToActivateSosScreen
        state
          { props
            { confirmTestDrill = true
            , triggeringSos = false
            , showTestDrill = false
            , showShimmer = true
            }
          }

eval ShowShareTripOptions state = continue state { props { showRideShareOptionsPopup = true } }

eval (ShareTripOptionPopup PopupWithCheckboxController.DismissPopup) state = continue state { props { showRideShareOptionsPopup = false } }

eval (ShareTripOptionPopup (PopupWithCheckboxController.ClickPrimaryButton PrimaryButtonController.OnClick)) state =
  exit
    $ PostEmergencySettings
        state
          { data { shareTripWithEmergencyContactOption = state.data.shareOptionCurrent }
          , props { showRideShareOptionsPopup = false }
          }

eval (ShareTripOptionPopup (PopupWithCheckboxController.ToggleSelect index)) state = 
  case (labelData DA.!! index) of
    Just option -> continue state { data { shareOptionCurrent = option.type } }
    Nothing -> continue state

eval _ state = continue state
