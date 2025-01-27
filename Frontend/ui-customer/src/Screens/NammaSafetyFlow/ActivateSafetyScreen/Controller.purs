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
import Prelude (class Show, bind, discard, map, not, pure, show, void, ($), (&&), (/=), (==), unit, (<>), (<))
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit)
import Screens.Types (NammaSafetyScreenState)
import Timers (clearTimerWithId)
import Common.Types.App as CTA
import Components.GenericHeader.Controller as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Data.Array as DA
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Screens.NammaSafetyFlow.Components.HeaderView as Header
import Screens.NammaSafetyFlow.Components.SafetyUtils (getDefaultPriorityList, getPrimaryContact)
import Screens.NammaSafetyFlow.ScreenData (defaultTimerValue)
import Services.API (ContactDetails(..), GetEmergencySettingsRes(..), Sos(..), SosFlow(..), RideShareOptions(..))
import Services.Config (getSupportNumber)
import PrestoDOM.Types.Core (class Loggable, defaultPerformLog)
import Components.SourceToDestination as SourceToDestination
import Common.Resources.Constants as Constants
import Screens.EmergencyContactsScreen.ScreenData (getRideOptionFromKeyEM)
import Components.Safety.SosButtonAndDescription as SosButtonAndDescription
import Components.Safety.SafetyActionTileView as SafetyActionTileView
import Components.OptionsMenu as OptionsMenu
import Components.Safety.SafetyAudioRecording as SafetyAudioRecording
import Engineering.Helpers.Utils as EHU
import PrestoDOM.Core (getPushFn)
import Effect.Uncurried (runEffectFn1, runEffectFn5, runEffectFn7, runEffectFn3)
import Timers (clearTimerWithId, waitingCountdownTimerV2Impl, startTimer)
import Engineering.Helpers.Commons as EHC
import Types.EndPoint as EndPoint
import Data.String as DS
import Data.Function.Uncurried (runFn3, runFn4)
import Components.PopupWithCheckbox.Controller as PopupWithCheckbox
import Services.API as API
import Services.Backend as Remote
import Engineering.Helpers.BackTrack (liftFlowBT)
import Effect.Aff (launchAff)
import Types.App (defaultGlobalState)
import Control.Monad.Except (runExceptT)
import Control.Transformers.Back.Trans (runBackT)
import Common.Types.App (LazyCheck (..))
import Helpers.Utils (emitTerminateApp, isParentView)
import Debug 

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

data ScreenOutput
  = GoBack NammaSafetyScreenState
  | GoToEmergencyContactScreen NammaSafetyScreenState
  | GoToSosScreen NammaSafetyScreenState
  | CreateSos NammaSafetyScreenState SosFlow
  | GoToEducationScreen NammaSafetyScreenState
  | GoToIssueScreen NammaSafetyScreenState
  | NotifyMockDrill NammaSafetyScreenState
  | GoToDataFetchScreen NammaSafetyScreenState

data Action
  = BackPressed
  | NoAction
  | SafetyHeaderAction Header.Action
  | CancelSosTrigger PrimaryButtonController.Action
  | AddContacts
  | UpdateEmergencySettings GetEmergencySettingsRes
  | DisableShimmer
  | CountDown Int String String
  | StartTestDrill PrimaryButtonController.Action
  | GoToSafetySettings
  | ContactAction ContactCircle.Action
  | GoToTestDrill
  | UpdateSosId Sos
  | GoToActiveSos
  | CallPolice
  | ShowSafetyIssueView
  | SelectedCurrentLocation Number Number String
  | GoToEducationView
  | CallSupport
  | SourceToDestinationAC SourceToDestination.Action
  | AlertSafetyTeam
  | SosButtonAndDescriptionAction SosButtonAndDescription.Action
  | RecordAudio SafetyActionTileView.Action
  | ShowPoliceView SafetyActionTileView.Action
  | CallSafetyTeam SafetyActionTileView.Action
  | ToggleSiren SafetyActionTileView.Action
  | OptionsMenuAction OptionsMenu.Action
  | SafetyAudioRecordingAction SafetyAudioRecording.Action
  | TimerCallback String String Int
  | UpdateRecording String
  | UpdateState NammaSafetyScreenState
  | UploadMultiPartDataCallback String String
  | PoliceTimerCallback Int String String
  | StopAudioPlayer
  | OnAudioCompleted String
  | PopupWithCheckboxAction PopupWithCheckbox.Action
  | DismissTestDrill PrimaryButtonController.Action
  | OnPauseCallback
  | AudioPermission Boolean
  | Exit ScreenOutput

eval :: Action -> NammaSafetyScreenState -> Eval Action ScreenOutput NammaSafetyScreenState
eval AddContacts state = updateAndExit state $ GoToEmergencyContactScreen state

eval (UpdateEmergencySettings (GetEmergencySettingsRes response)) state = do
  let
    contacts =
      map
      ( \(ContactDetails item) ->
          { number: item.mobileNumber
          , name: item.name
          , isSelected: item.priority == Just 0
          , enableForFollowing: fromMaybe false item.enableForFollowing
          , enableForShareRide: fromMaybe false item.enableForShareRide
          , shareTripWithEmergencyContactOption: getRideOptionFromKeyEM $ fromMaybe NEVER_SHARE item.shareTripWithEmergencyContactOption
          , onRide : fromMaybe false item.onRide
          , priority: fromMaybe 1 item.priority
          , contactPersonId : item.contactPersonId
          , isFollowing : Nothing
          , notifiedViaFCM : item.notifiedViaFCM
          }
      )
      response.defaultEmergencyNumbers
  continue
    state
      { data
        { hasCompletedSafetySetup = response.hasCompletedSafetySetup
        , nightSafetyChecks = response.nightSafetyChecks
        , hasCompletedMockSafetyDrill = response.hasCompletedMockSafetyDrill
        , emergencyContactsList = getDefaultPriorityList contacts
        , autoCallDefaultContact = response.autoCallDefaultContact
        }
      , props { enableLocalPoliceSupport = response.enablePoliceSupport, localPoliceNumber = fromMaybe "" response.localPoliceNumber }
      }

eval (SafetyHeaderAction (Header.GenericHeaderAC GenericHeaderController.PrefixImgOnClick)) state = continueWithCmd state [ pure BackPressed ]

eval (SafetyHeaderAction (Header.BackClicked)) state = continueWithCmd state [ pure BackPressed ]

eval (SafetyHeaderAction (Header.LearnMoreClicked)) state = do
  _ <- pure $ clearTimerWithId state.props.timerId
  exit $ GoToEducationScreen state { props { triggeringSos = false, timerValue = defaultTimerValue, timerId = "", confirmTestDrill = false } }

eval (SafetyHeaderAction (Header.OptionsMenuToggle)) state = continue state{props{showMenu = not state.props.showMenu}}

eval (CancelSosTrigger PrimaryButtonController.OnClick) state = do
  _ <- pure $ clearTimerWithId state.props.timerId
  pure $ JB.clearAudioPlayer ""
  continue state { props { triggeringSos = false, timerValue = defaultTimerValue, timerId = "" , triggerSiren = false } }

eval BackPressed state = do
  pure $ JB.clearAudioPlayer ""
  void $ pure $ clearTimerWithId state.props.recordingTimerId
  let newState = state { props { triggerSiren = false } }
  if state.props.showCallPolice then do
    void $ pure $ spy "BackPressed ActivateSafetyScreen" "If"
    void $ pure $ clearTimerWithId state.props.policeCallTimerId
    if state.props.isSafetyCenterDisabled 
      then exit $ GoBack newState
      else continue newState { props { showCallPolice = false, policeCallTimerValue = 5} }
  else 
    if isParentView FunctionCall 
    then do 
      void $ pure $ spy "BackPressed ActivateSafetyScreen" "Else If"
      void $ pure $ emitTerminateApp Nothing true
      continue state
    else do
      void $ pure $ spy "BackPressed ActivateSafetyScreen" "else"
      continueWithCmd state [do
        if state.props.isAudioRecordingActive then do
          void $ runEffectFn1 JB.removeMediaPlayer ""
          void $ runEffectFn1 JB.stopAudioRecording ""
          pure unit
        else
          pure unit
        pure $ Exit $ GoBack newState
      ]

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


eval AlertSafetyTeam state = exit $ CreateSos state SafetyFlow

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
        { confirmTestDrill = false
        , showTestDrill = true
        , timerValue = defaultTimerValue
        , triggeringSos = false
        , isAudioRecordingActive = false
        }
      }

eval GoToActiveSos state = 
  exit $ GoToSosScreen state { props { triggeringSos = false, timerValue = defaultTimerValue, timerId = "" } }

eval (UpdateSosId (Sos sos)) state = do
  if sos.flow == SafetyFlow && DA.notElem sos.status [CTA.Resolved, CTA.MockResolved] then do
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

eval (ShowPoliceView SafetyActionTileView.OnClick) state = continueWithCmd state { props { showCallPolice = true } } [ do
    push <- getPushFn Nothing "ActivateSafetyScreen"
    void $ startTimer state.props.policeCallTimerValue "policeCallTimer" "1" push PoliceTimerCallback
    pure NoAction
  ]

eval CallPolice state = do
  void $ pure $ JB.showDialer Constants.policeNumber false
  exit $ CreateSos state Police

eval ShowSafetyIssueView state = exit $ GoToIssueScreen state

eval (SelectedCurrentLocation lat lon name) state = continue state { data { currentLocation = name, currentLatLon = Just $ API.LatLong {lat : lat, lon : lon} } }

eval GoToEducationView state = do
  _ <- pure $ clearTimerWithId state.props.timerId
  exit $ GoToEducationScreen state { props { triggeringSos = false, timerValue = defaultTimerValue, timerId = "" } }

eval CallSupport state = do
  void $ pure $ JB.showDialer (getSupportNumber "") false
  continue state

eval (SosButtonAndDescriptionAction SosButtonAndDescription.TriggerSosCountdown) state = continue state { props { triggeringSos = true } }

eval (SosButtonAndDescriptionAction SosButtonAndDescription.AddContacts) state = if (isJust $ getPrimaryContact state) && state.data.autoCallDefaultContact
  then continue state{props{defaultCallPopup = true}}
  else exit $ GoToDataFetchScreen state

eval (SosButtonAndDescriptionAction (SosButtonAndDescription.CountDown seconds status timerID)) state = do
  _ <- pure $ printLog "timer" $ show seconds
  if status == "EXPIRED" then do
    _ <- pure $ clearTimerWithId state.props.timerId
    pure $ JB.clearAudioPlayer ""
    let
      newState = state { props { timerId = "", triggerSiren = false, policeCallTimerValue = 5, policeCallTimerId = "", audioRecordingStatus = CTA.NOT_RECORDING, recordingTimer = "00 : 00", isAudioRecordingActive = false  } }
    updateAndExit newState $ CreateSos newState SafetyFlow
  else if seconds == 3 && state.props.triggerSiren then handleMediaPlayerRestart state { props { timerValue = seconds, timerId = timerID } }
  else do
    if seconds < 3 && not state.props.triggerSiren then do
      pure $ JB.clearAudioPlayer ""
    else pure unit
    continue state { props { timerValue = seconds, timerId = timerID } }

eval (ToggleSiren SafetyActionTileView.OnClick) state = 
    if state.props.triggerSiren then
      continueWithCmd state { props { triggerSiren = false } }
      [ pure StopAudioPlayer ]
    else
      continue state { props { triggerSiren = true } }

eval (OptionsMenuAction OptionsMenu.BackgroundClick) state = continue state{props{showMenu = false}}

eval (OptionsMenuAction (OptionsMenu.ItemClick item)) state = do
  let newState = state{props{showMenu = false}}
  case item of
    "report_safety_issue" -> exit $ GoToIssueScreen state
    "start_test_drill" -> continueWithCmd newState [pure $ GoToTestDrill]
    "learn_about_safety" -> continueWithCmd newState [pure $ GoToEducationView]
    _ -> continue newState

eval (SafetyAudioRecordingAction SafetyAudioRecording.StartRecord) state = 
  continueWithCmd state { props { recordingTimer = "00 : 00", audioRecordingStatus = CTA.RECORDING  } }  [do
    push <- getPushFn Nothing "ActivateSafetyScreen"
    let extension = if EHC.os == "IOS" then ".wav" else ".mp3"
        audioPath = if state.props.showTestDrill then "" else ("/safety-recording/" <> state.data.rideId <> "/" <> EHC.getCurrentUTC "" <> extension)

    recordingStarted <- runEffectFn1 JB.startAudioRecording audioPath
    if recordingStarted then do
      void $ pure $ clearTimerWithId state.props.recordingTimerId
      void $ runEffectFn1 JB.removeMediaPlayer ""
      void $ runEffectFn5 waitingCountdownTimerV2Impl 0 "1" "record_issue_audio" push TimerCallback
      pure $ UpdateState state { props { recordingTimer = "00 : 00", audioRecordingStatus = CTA.RECORDING } }
    else
      pure $ NoAction
  ]

eval (SafetyAudioRecordingAction SafetyAudioRecording.StopRecord) state = 
  continueWithCmd state { props { audioRecordingStatus = CTA.RECORDED, recordingTimer = "00 : 00"  } } [do
    _   <- pure $ clearTimerWithId state.props.recordingTimerId
    res <- runEffectFn1 JB.stopAudioRecording ""
    pure $ UpdateRecording res
  ]

eval (UpdateRecording url) state = do
  continueWithCmd state { props { recordedAudioUrl = Just url } } [do
    void $ runEffectFn7 JB.addMediaFile (EHC.getNewIDWithTag "recordedAudioViewUniqueOne") url (EHC.getNewIDWithTag "recordedAudiobtn") "ny_ic_play_black_white" "ny_ic_stop_record" (EHC.getNewIDWithTag "recordedAudiotimer") false
    pure NoAction
  ]

eval (SafetyAudioRecordingAction (SafetyAudioRecording.ShareAudio PrimaryButtonController.OnClick)) state = continueWithCmd state [ do
    case state.props.recordedAudioUrl of
      Just url -> do
        void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
          (API.UserSosRes res) <- Remote.userSosBT (Remote.makeUserSosReq (Remote.createUserSosFlow "AudioRecording" "") state.data.rideId false false Nothing Nothing)
          liftFlowBT $ void $ runEffectFn5 JB.uploadMultiPartData url (EndPoint.updateSosMedia res.sosId) "Audio" "fileUrl" "payload"
      Nothing -> pure unit
    pure NoAction
  ]

eval (TimerCallback timerId timeInMinutes seconds) state = 
   continue state { props { recordingTimer = timeInMinutes, recordingTimerId = timerId } }

eval (UpdateState newState) state = continue newState

eval (RecordAudio SafetyActionTileView.OnClick) state = continueWithCmd state {props{isAudioRecordingActive = EHC.os == "IOS"}} [do
  push <- getPushFn Nothing "ActivateSafetyScreen"
  if EHC.os == "IOS" then do
    pure $ SafetyAudioRecordingAction SafetyAudioRecording.StartRecord
  else do
    let _ = JB.askRequestedPermissionsWithCallback [ "android.permission.RECORD_AUDIO" ] push AudioPermission
    pure $ UpdateState state
]

eval (AudioPermission status) state = 
  if status then 
    continueWithCmd state{props{isAudioRecordingActive = status}} [pure $ SafetyAudioRecordingAction SafetyAudioRecording.StartRecord]
  else
    continue state{props{isAudioRecordingActive = false}}

eval (UploadMultiPartDataCallback _ fileId) state = do
  void $ pure $ JB.toggleBtnLoader "" false
  if DS.null fileId then do
    void $ pure $ EHU.showToast "Failed to upload media file. Please try again."
    continue state 
  else do
    void $ pure $ EHU.showToast "Audio shared successfully"
    continue state {props { recordedAudioUrl = Nothing, audioRecordingStatus = CTA.NOT_RECORDING, recordingTimer = "00 : 00", isAudioRecordingActive = false} }

eval (SafetyAudioRecordingAction SafetyAudioRecording.CancelAudioRecording) state = 
  continueWithCmd state { props { audioRecordingStatus = CTA.NOT_RECORDING, recordingTimer = "00 : 00", isAudioRecordingActive = false } } [do
    _   <- pure $ clearTimerWithId state.props.recordingTimerId
    void $ runEffectFn1 JB.removeMediaPlayer ""
    void $ runEffectFn1 JB.stopAudioRecording ""
    pure NoAction
  ]

eval (PoliceTimerCallback seconds status timerID) state = do
  if status == "EXPIRED" then do
    void $ pure $ clearTimerWithId state.props.policeCallTimerId
    void $ pure $ JB.showDialer Constants.policeNumber false
    continueWithCmd state { props { showCallPolice = false, policeCallTimerValue = 5, policeCallTimerId = ""} } [pure CallPolice]
  else
    continue state { props { policeCallTimerValue = seconds, policeCallTimerId = timerID } }
  
eval (CallSafetyTeam SafetyActionTileView.OnClick) state = do
  void $ pure $ JB.showDialer state.data.config.safety.safetyTeamNumber true
  exit $ CreateSos state CustomerCare

eval (ToggleSiren SafetyActionTileView.OnClick) state = 
  if state.props.triggerSiren then
    continueWithCmd state { props { triggerSiren = false } }
      [ pure StopAudioPlayer ]
  else
    handleMediaPlayerRestart state

eval StopAudioPlayer state = do
  _ <- pure $ JB.clearAudioPlayer ""
  continue state { props { triggerSiren = false } }

eval (OnAudioCompleted _) state = handleMediaPlayerRestart state

eval (PopupWithCheckboxAction (PopupWithCheckbox.ToggleSelect index)) state = do
  let
    newContacts =
      DA.mapWithIndex
        ( \i contact -> contact { isSelected = i == index, priority = if i == index then 0 else 1 }
        )
        state.data.emergencyContactsList
  continue state { data { emergencyContactsList = newContacts } }

eval (PopupWithCheckboxAction (PopupWithCheckbox.DismissPopup)) state = continue state{props{defaultCallPopup = false}}

eval (DismissTestDrill PrimaryButtonController.OnClick) state = do
  _ <- pure $ JB.clearAudioPlayer ""
  _ <- pure $ clearTimerWithId state.props.recordingTimerId
  continueWithCmd state [do
    void $ runEffectFn1 JB.removeMediaPlayer ""
    void $ runEffectFn1 JB.stopAudioRecording ""
    pure $ BackPressed
  ]

eval OnPauseCallback state = continueWithCmd state [do
  pure $ SafetyAudioRecordingAction SafetyAudioRecording.CancelAudioRecording
]

eval (Exit output) state = exit output
  
eval _ state = update state

handleMediaPlayerRestart :: NammaSafetyScreenState -> Eval Action ScreenOutput NammaSafetyScreenState
handleMediaPlayerRestart state = continueWithCmd state { props { triggerSiren = true } }
        [ do
            push <- getPushFn Nothing "SosActiveScreen"
            void $ pure $ runFn4 JB.startAudioPlayer "ny_ic_sos_danger_full" push OnAudioCompleted "0"
            pure NoAction
        ]