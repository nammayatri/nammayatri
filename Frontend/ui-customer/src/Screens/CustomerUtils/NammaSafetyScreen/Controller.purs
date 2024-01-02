{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyScreen.Controller where

import Prelude
import PrestoDOM
import Components.GenericHeader.Controller as GenericHeaderController
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton.Controller as PrimaryButtonController
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
import Engineering.Helpers.Utils (loaderText, toggleLoader, uploadMultiPartData, uploadMultiPartDataIOS)
import Helpers.Utils as HU
import JBridge (askRequestedPermissions, openUrlInApp, showDialer, stopRecord, getLayoutBounds)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog, trackAppActionClick, trackAppBackPress, trackAppEndScreen, trackAppScreenRender)
import Presto.Core.Types.Language.Flow (delay)
import PrestoDOM.Core (getPushFn)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (NammaSafetyScreenState, NammaSafetyStage(..), NewContacts, RecordingState(..))
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
    GenericHeaderAC act -> case act of
      GenericHeaderController.PrefixImgOnClick -> do
        trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "generic_header_action" "back_icon"
        trackAppEndScreen appId (getScreen NAMMASAFETY_SCREEN)
      GenericHeaderController.SuffixImgOnClick -> trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "generic_header_action" "forward_icon"
    StartNammaSafetyOnboarding act -> case act of
      PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "start_onboarding" "primary button"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "no_action" "primary button"
    GoToNextStep act -> case act of
      PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "next_step_onboard" "primary button"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen NAMMASAFETY_SCREEN) "no_action" "primary button"
    _ -> trackAppScreenRender appId "screen" (getScreen NAMMASAFETY_SCREEN)

data ScreenOutput
  = GoBack NammaSafetyScreenState
  | PostContacts NammaSafetyScreenState
  | Refresh NammaSafetyScreenState
  | PostEmergencySettings NammaSafetyScreenState Boolean
  | CreateSOS NammaSafetyScreenState
  | UpdateAction NammaSafetyScreenState
  | UpdateSafe NammaSafetyScreenState
  | GoToEmergencyContactScreen NammaSafetyScreenState
  | GoToEmergencyVideo NammaSafetyScreenState

data Action
  = BackPressed
  | NoAction
  | StepsHeaderModelAC StepsHeaderModelController.Action
  | GenericHeaderAC GenericHeaderController.Action
  | StartNammaSafetyOnboarding PrimaryButtonController.Action
  | GoToNextStep PrimaryButtonController.Action
  | EditEmergencyContacts PrimaryButtonController.Action
  | SwitchToStage NammaSafetyStage
  | ToggleSwitch NammaSafetyStage
  | ActivateSOS PrimaryButtonController.Action
  | CallForSupport String
  | UpdateSosId String
  | PopUpModalAction PopUpModal.Action
  | ContactListPrimaryButtonActionController PrimaryButtonController.Action
  | RemoveButtonClicked NewContacts
  | AddEmergencyContacts PrimaryButtonController.Action
  | AddContacts
  | UpdateEmergencySettings GetEmergencySettingsRes
  | ConfirmSOSActivate PopUpModal.Action
  | MarkRideAsSafe PrimaryButtonController.Action
  | DismissSOS PrimaryButtonController.Action
  | ChangeRecordingState RecordingState
  | ActivateSoSAndCallPolice
  | ShareSilentSos (Maybe String)
  | GoBackToActivate PrimaryButtonController.Action
  | DisableShimmer
  | GoToVideoRecord
  | PermissionsCallback Boolean

eval :: Action -> NammaSafetyScreenState -> Eval Action ScreenOutput NammaSafetyScreenState
eval (AddEmergencyContacts PrimaryButtonController.OnClick) state = continueWithCmd state [ pure AddContacts ]

eval AddContacts state = updateAndExit state $ GoToEmergencyContactScreen state

eval (UpdateSosId sosId) state = do
  let
    newStage =
      if checkForContactsAndSupportDisabled state then
        ActivateNammaSafety
      else
        TriggeredNammaSafety
  void $ pure $ setValueToLocalStore IS_SOS_ACTIVE "true"
  continue
    state
      { data { sosId = sosId }
      , props
        { currentStage = newStage
        }
      }

eval (UpdateEmergencySettings (GetEmergencySettingsRes response)) state = do
  let
    contacts =
      map
        ( \(ContactDetails item) ->
            { number: item.mobileNumber
            , name: item.name
            , isSelected: true
            }
        )
        response.defaultEmergencyNumbers
  continue
    state
      { data
        { hasCompletedSafetySetup = response.hasCompletedSafetySetup
        , shareToEmergencyContacts = response.shareEmergencyContacts
        , nightSafetyChecks = response.nightSafetyChecks
        , triggerSupport = response.triggerSupport
        , contactsList = contacts
        }
      , props { enableLocalPoliceSupport = response.enablePoliceSupport, localPoliceNumber = fromMaybe "" response.localPoliceNumber }
      }

eval (StepsHeaderModelAC StepsHeaderModelController.OnArrowClick) state = continueWithCmd state [ pure BackPressed ]

eval (GenericHeaderAC (GenericHeaderController.PrefixImgOnClick)) state = continueWithCmd state [ pure BackPressed ]

eval (GenericHeaderAC (GenericHeaderController.SuffixImgOnClick)) state = 
  continueWithCmd state { props { timerValue = 15, recordingState = NOT_RECORDING } } [ do pure $ SwitchToStage TriggeredNammaSafety ]

eval (ToggleSwitch stage) state = 
  if state.props.currentStage == NammaSafetyDashboard then do
    case stage of
      SetTriggerCustomerSupport -> exit $ PostEmergencySettings state { data { triggerSupport = not state.data.triggerSupport } } false
      SetNightTimeSafetyAlert -> exit $ PostEmergencySettings state { data { nightSafetyChecks = not state.data.nightSafetyChecks } } false
      SetDefaultEmergencyContacts ->
        if DA.length state.data.contactsList /= 0 then
          exit $ PostEmergencySettings state { data { shareToEmergencyContacts = not state.data.shareToEmergencyContacts } } true
        else
          continueWithCmd state [ pure AddContacts ]
      _ -> continue state
  else do
    case stage of
      SetTriggerCustomerSupport -> continue state { data { triggerSupport = not state.data.triggerSupport } }
      SetNightTimeSafetyAlert -> continue state { data { nightSafetyChecks = not state.data.nightSafetyChecks } }
      SetDefaultEmergencyContacts ->
        if DA.length state.data.contactsList /= 0 then
          continue state { data { shareToEmergencyContacts = not state.data.shareToEmergencyContacts } }
        else
          continueWithCmd state [ pure AddContacts ]
      _ -> continue state

eval (MarkRideAsSafe PrimaryButtonController.OnClick) state = exit $ UpdateSafe state

eval (ActivateSOS PrimaryButtonController.OnClick) state = continue state { props { confirmPopup = true } }

eval (StartNammaSafetyOnboarding PrimaryButtonController.OnClick) state = continue state { props { currentStage = SetDefaultEmergencyContacts } }

eval (EditEmergencyContacts PrimaryButtonController.OnClick) state = updateAndExit state $ GoToEmergencyContactScreen state

eval (SwitchToStage stage) state = case stage of
  NammaSafetyVideoRecord -> 
    continue state { props { currentStage = NammaSafetyVideoRecord, timerValue = 15, recordingState = NOT_RECORDING, shareTimerValue = 5 } }
  _ -> continue state { props { currentStage = stage } }

eval (CallForSupport callTo) state = do
  void <- pure $ showDialer (if callTo == "police" then "112" else getSupportNumber "") false
  exit $ UpdateAction state { data { updateActionType = callTo } }

eval (DismissSOS PrimaryButtonController.OnClick) state = exit $ GoBack state

eval (GoToNextStep PrimaryButtonController.OnClick) state = do
  case state.props.currentStage of
    SetTriggerCustomerSupport -> continue state { props { currentStage = SetNightTimeSafetyAlert } }
    SetNightTimeSafetyAlert -> continue state { props { currentStage = SetPersonalSafetySettings } }
    SetDefaultEmergencyContacts -> continue state { props { currentStage = if state.data.safetyConfig.enableSupport then SetTriggerCustomerSupport else SetNightTimeSafetyAlert } }
    SetPersonalSafetySettings -> do
      if state.data.safetyConfig.enableSupport || state.props.enableLocalPoliceSupport then do
        if EHC.os == "IOS" then do
          void $ pure $ HU.requestCameraAndMicrophonePermissions unit
        else
          void $ pure $ askRequestedPermissions [ "android.permission.CAMERA", "android.permission.RECORD_AUDIO" ]
      else
        pure unit
      exit $ PostEmergencySettings state false
    _ -> continue state

eval (PopUpModalAction PopUpModal.OnButton2Click) state = do
  let
    newContacts = DA.filter (\x -> x.number <> x.name /= state.data.removedContactDetail.number <> state.data.removedContactDetail.name) state.data.contactsList
  contactsInString <- pure $ HU.toStringJSON newContacts
  void $ pure $ setValueToLocalStore CONTACTS contactsInString
  exit $ PostContacts state { data { contactsList = newContacts } }

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue state { props { showInfoPopUp = false } }

eval (RemoveButtonClicked contactDetail) state = continue state { props { showInfoPopUp = true }, data { removedContactDetail = contactDetail } }

eval (BackPressed) state = do
  if state.props.confirmPopup then
    continue state { props { confirmPopup = false } }
  else case state.props.currentStage of
    NammaSafetyDashboard -> case state.props.onRide, state.data.sosId == "" of
      true, true -> continue state { props { currentStage = ActivateNammaSafety } }
      true, false -> continue state { props { currentStage = if checkForContactsAndSupportDisabled state then ActivateNammaSafety else TriggeredNammaSafety } }
      false, _ -> exit $ GoBack state
    AboutNammaSafety -> continue state { props { currentStage = if state.props.onRide then ActivateNammaSafety else NammaSafetyDashboard } }
    SetTriggerCustomerSupport -> continue state { props { currentStage = SetDefaultEmergencyContacts } }
    SetNightTimeSafetyAlert -> continue state { props { currentStage = if state.data.safetyConfig.enableSupport then SetTriggerCustomerSupport else SetDefaultEmergencyContacts } }
    SetDefaultEmergencyContacts -> continue state { props { currentStage = NammaSafetyDashboard } }
    SetPersonalSafetySettings -> continue state { props { currentStage = SetNightTimeSafetyAlert } }
    EduNammaSafetyMeasures -> continue state { props { currentStage = AboutNammaSafety } }
    EduNammaSafetyGuidelines -> continue state { props { currentStage = AboutNammaSafety } }
    EduNammaSafetyAboutSOS -> continue state { props { currentStage = AboutNammaSafety } }
    ActivateNammaSafety -> exit $ GoBack state
    TriggeredNammaSafety -> exit $ GoBack state
    NammaSafetyVideoRecord -> 
      continue state { props { currentStage = TriggeredNammaSafety, timerValue = 15, recordingState = NOT_RECORDING, shareTimerValue = 5 } }
    EmergencyContactsStage ->
      if state.data.hasCompletedSafetySetup || state.props.onRide then
        continue state { props { currentStage = NammaSafetyDashboard } }
      else
        continue state { props { currentStage = SetDefaultEmergencyContacts } }
    _ -> continue state

eval (ConfirmSOSActivate (PopUpModal.OnButton1Click)) state = updateAndExit state { props { confirmPopup = false } } $ CreateSOS state { props { confirmPopup = false } }

eval (ConfirmSOSActivate (PopUpModal.OnButton2Click)) state = continue state { props { confirmPopup = false } }

eval ActivateSoSAndCallPolice state = do
  void $ pure $ showDialer "112" false
  if state.data.sosId == "" then
    exit $ CreateSOS state
  else
    continue state

eval (ShareSilentSos videoUri) state =
  continueWithCmd state
    [ do
        void $ openUrlInApp $ constructWhatsappMessage videoUri state
        pure NoAction
    ]


eval (GoBackToActivate PrimaryButtonController.OnClick) state = continueWithCmd state [ pure BackPressed ]

eval DisableShimmer state = continue state { props { showShimmer = false } }

eval GoToVideoRecord state = do
  let
    newState = state { props { currentStage = NammaSafetyVideoRecord, recordingState = NOT_RECORDING, shareTimerValue = 5, timerValue = 15 } }
  updateAndExit newState $ GoToEmergencyVideo newState

eval (PermissionsCallback isPermissionGranted) state = do
  if isPermissionGranted then
    continueWithCmd state [ pure GoToVideoRecord ]
  else
    continueWithCmd state [ do 
    _ <- pure $ toast (getString PLEASE_ALLOW_CAMERA_AND_MICROPHONE_PERMISSIONS)
    pure NoAction
  ]

eval (_) state = continue state

checkForContactsAndSupportDisabled :: NammaSafetyScreenState -> Boolean
checkForContactsAndSupportDisabled state = do
  (DA.null state.data.contactsList || not state.data.shareToEmergencyContacts) && not state.data.safetyConfig.enableSupport && not state.props.enableLocalPoliceSupport

constructWhatsappMessage :: Maybe String -> NammaSafetyScreenState -> String
constructWhatsappMessage videoUri state = do
  let
    uriText = case videoUri of
      Just uri -> "\n\nSOS Video Link : " <> uri
      Nothing -> ""

    dataToEncode =
      EHC.encodeURIData $ "*SOS Alert*\n"
        <> "I am in an emergency during Namma Yatri ride.\n\nHere are my details:\nUsername : "
        <> getValueToLocalStore USER_NAME
        <> "\nPhone number : "
        <> getValueToLocalStore MOBILE_NUMBER
        <> "\nRide Journey link - https://nammayatri.in/track/?id="
        <> state.data.rideId
        <> uriText
  "https://wa.me/" <> DS.trim state.props.localPoliceNumber <> "?text=" <> dataToEncode
