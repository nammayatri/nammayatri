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
import Prelude 
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit)
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
import Services.API (ContactDetails(..), GetEmergencySettingsRes(..), RideShareOptions(..), RideBookingListRes(..))
import PrestoDOM.Types.Core (class Loggable, defaultPerformLog)
import Engineering.Helpers.Commons (getExpiryTime)
import Data.Lens ((^.))
import Accessor (_rideEndTime)
import Components.PopUpModal as PopUpModal
import Screens.RideSelectionScreen.Transformer (myRideListTransformer)
import Helpers.Utils as HU
import Common.Resources.Constants as Constants
import Screens.EmergencyContactsScreen.ScreenData (getRideOptionFromKeyEM)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

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
  | CheckRideListResp RideBookingListRes
  | DisableShimmer
  | ChangeFollowing Int
  | GoToEducationView
  | StartTestDrill PrimaryButtonController.Action
  | ContactAction ContactCircle.Action
  | ShowShareTripOptions
  | ShareTripOptionPopup PopupWithCheckboxController.Action
  | PopUpModalAC PopUpModal.Action
  | DialPolice

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
            , shareTripWithEmergencyContactOption: getRideOptionFromKeyEM $ fromMaybe NEVER_SHARE item.shareTripWithEmergencyContactOption
            , priority: fromMaybe 1 item.priority
            , onRide : fromMaybe false item.onRide
            , contactPersonId : item.contactPersonId
            , isFollowing : Nothing
            , notifiedViaFCM : item.notifiedViaFCM
            }
        )
        response.defaultEmergencyNumbers
  void $ pure $ JB.setCleverTapUserProp [ { key: "Safety Setup Completed", value: unsafeToForeign response.hasCompletedSafetySetup } ]
  -- void $ pure $ JB.setCleverTapUserProp [ { key: "Auto Share Night Ride", value: unsafeToForeign response.shareTripWithEmergencyContacts } ]
  void $ pure $ JB.setCleverTapUserProp [ { key: "Mock Safety Drill Completed", value: unsafeToForeign response.hasCompletedMockSafetyDrill } ]
  void $ pure $ JB.setCleverTapUserProp [ { key: "Night Safety Check Enabled", value: unsafeToForeign response.nightSafetyChecks } ]
  continue
    state
      { data
        { hasCompletedSafetySetup = response.hasCompletedSafetySetup
        , nightSafetyChecks = response.nightSafetyChecks
        , hasCompletedMockSafetyDrill = response.hasCompletedMockSafetyDrill
        , emergencyContactsList = getDefaultPriorityList contacts
        }
      , props { enableLocalPoliceSupport = response.enablePoliceSupport, localPoliceNumber = fromMaybe "" response.localPoliceNumber }
      }

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
            { confirmTestDrill = false
            , triggeringSos = false
            , showTestDrill = true
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

eval (PopUpModalAC PopUpModal.OnButton1Click) state = do
  let newState = state{props{showPastRidePopUp = false, reportPastRide = true}}
  exit $ GoToActivateSosScreen newState

eval (PopUpModalAC PopUpModal.OnButton2Click) state = continue state{props{showPastRidePopUp = false, checkPastRide = false}}

eval (PopUpModalAC PopUpModal.DismissPopup) state = continue state{props{showPastRidePopUp = false}}

eval (CheckRideListResp (RideBookingListRes listResp)) state = do
  let mbResp = DA.head listResp.list
  case mbResp of
    Nothing -> continue state
    Just resp -> do
      let isRecentRide = getExpiryTime (fromMaybe "" (resp ^. _rideEndTime)) true / 60 < state.data.config.safety.pastRideInterval
          transformedResp = myRideListTransformer true listResp.list state.data.config Nothing
          mbRideData = DA.head transformedResp
      continue state{props{showPastRidePopUp = isRecentRide}, data{lastRideDetails = mbRideData}}

eval DialPolice state = do
   void $ pure $ HU.performHapticFeedback unit
   pure $ JB.showDialer Constants.policeNumber false
   continue state

eval _ state = update state
