{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.SosActiveScreen.Controller where

import Data.Maybe (Maybe(..))
import Log (trackAppBackPress, trackAppScreenRender)
import Prelude (class Show, discard, pure, unit, void, ($), (==), bind, not, (<>))
import PrestoDOM (Eval, update, continue, continueWithCmd, exit)
import Screens.Types as ST
import Components.GenericHeader.Controller as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Data.Array as DA
import JBridge as JB
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Screens.NammaSafetyFlow.Components.HeaderView as Header
import Services.API (GetEmergencySettingsRes)
import PrestoDOM.Types.Core (class Loggable, defaultPerformLog)
import Components.Safety.SosButtonAndDescription as SosButtonAndDescription
import Components.Safety.SafetyActionTileView as SafetyActionTileView
import PrestoDOM.Core (getPushFn)
import Data.Function.Uncurried (runFn3)
import Effect.Uncurried (runEffectFn1, runEffectFn5, runEffectFn7, runEffectFn3)
import Timers (clearTimerWithId, waitingCountdownTimerV2Impl, startTimer)
import Engineering.Helpers.Commons as EHC
import Types.EndPoint as EndPoint
import Data.String as DS
import Engineering.Helpers.Utils as EHU
import Common.Types.App as CTA
import Components.Safety.SafetyAudioRecording as SafetyAudioRecording
import Common.Resources.Constants as Constants
import Data.Function.Uncurried (runFn3, runFn4)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

data ScreenOutput
  = GoBack ST.NammaSafetyScreenState
  | UpdateAsSafe ST.NammaSafetyScreenState
  | GoToEducationScreen ST.NammaSafetyScreenState
  | UpdateAction ST.NammaSafetyScreenState String

data Action
  = BackPressed
  | NoAction
  | SafetyHeaderAction Header.Action
  | UpdateEmergencySettings GetEmergencySettingsRes
  | DisableShimmer
  | MarkRideAsSafe PrimaryButtonController.Action
  | CallContact Int
  | CallPolice
  | ShowPoliceView SafetyActionTileView.Action
  | LearnMoreClicked
  | PlaceCall
  | SelectedCurrentLocation Number Number String
  | ContactCircleAction ContactCircle.Action
  | SosButtonAndDescriptionAction SosButtonAndDescription.Action
  | RecordAudio SafetyActionTileView.Action
  | ToggleSiren SafetyActionTileView.Action
  | CallSafetyTeam SafetyActionTileView.Action
  | OnAudioCompleted String
  | StopAudioPlayer
  | StartRecord
  | StopRecord
  | TimerCallback String String Int
  | UpdateRecording String
  | UpdateState ST.NammaSafetyScreenState
  | ShareAudio PrimaryButtonController.Action
  | UploadMultiPartDataCallback String String
  | CancelAudioRecording
  | SafetyAudioRecordingAction SafetyAudioRecording.Action
  | PoliceTimerCallback Int String String
  | OnPauseCallback
  | AudioPermission Boolean
  | Exit ScreenOutput

eval :: Action -> ST.NammaSafetyScreenState -> Eval Action ScreenOutput ST.NammaSafetyScreenState

eval PlaceCall state = do
  let
    primaryContact = DA.filter (\item -> item.priority == 0) state.data.emergencyContactsList
  case primaryContact DA.!! 0, state.props.shouldCallAutomatically, state.data.autoCallDefaultContact of
    Just contact, true, true -> void $ pure $ JB.showDialer contact.number true
    _, _, _ -> pure unit
  continue
    state
      { props
        { shouldCallAutomatically = false
        }
      }

eval (SafetyHeaderAction (Header.GenericHeaderAC GenericHeaderController.PrefixImgOnClick)) state = continueWithCmd state [ pure BackPressed ]

eval (SafetyHeaderAction (Header.BackClicked)) state = continueWithCmd state [ pure BackPressed ]

eval (SafetyHeaderAction (Header.LearnMoreClicked)) state = exit $ GoToEducationScreen state

eval (BackPressed) state = do
  pure $ JB.clearAudioPlayer ""
  _ <- pure $ clearTimerWithId state.props.recordingTimerId
  let newState = state { props { triggerSiren = false } }
  if state.props.showCallPolice then do
    void $ pure $ clearTimerWithId state.props.policeCallTimerId
    continue newState { props { showCallPolice = false, policeCallTimerValue = 5 } }
  else 
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

eval (CallContact contactIndex) state = do
  pure $ JB.clearAudioPlayer ""
  let newState = state { props { triggerSiren = false } }
  case state.data.emergencyContactsList DA.!! contactIndex of
    Just item -> void $ pure $ JB.showDialer item.number true
    Nothing -> pure unit
  if state.props.showTestDrill then
    continue newState
  else
    exit $ UpdateAction newState "Called Emergency Contact"

eval CallPolice state = do
  void $ pure $ JB.showDialer "112" false
  exit $ UpdateAction state "Called Police"

eval (MarkRideAsSafe PrimaryButtonController.OnClick) state = do
  _ <- pure $ clearTimerWithId state.props.recordingTimerId
  _ <- pure $ JB.clearAudioPlayer ""
  continueWithCmd state [do
    void $ runEffectFn1 JB.removeMediaPlayer ""
    void $ runEffectFn1 JB.stopAudioRecording ""
    pure $ Exit $ UpdateAsSafe state
  ]

eval (SelectedCurrentLocation _ _ name) state = continue state { data { currentLocation = name } }

eval (ToggleSiren SafetyActionTileView.OnClick) state = 
  if state.props.triggerSiren then
    continueWithCmd state { props { triggerSiren = false } }
      [ pure StopAudioPlayer ]
  else
    handleMediaPlayerRestart state

eval StopAudioPlayer state = do
  _ <- pure $ JB.clearAudioPlayer ""
  continue state { props { triggerSiren = false } }

eval (OnAudioCompleted _) state = if state.props.triggerSiren then handleMediaPlayerRestart state else continue state

eval (RecordAudio SafetyActionTileView.OnClick) state = continueWithCmd state{props{isAudioRecordingActive = EHC.os == "IOS"}} [do
  push <- getPushFn Nothing "SosActiveScreen"
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

eval (UpdateState newState) state = continue newState

eval (TimerCallback timerId timeInMinutes seconds) state = 
   continue state { props { recordingTimer = timeInMinutes, recordingTimerId = timerId } }

eval (SafetyAudioRecordingAction SafetyAudioRecording.StartRecord) state = 
  continueWithCmd state { props { recordingTimer = "00 : 00", audioRecordingStatus = CTA.RECORDING  } }  [do
    push <- getPushFn Nothing "SosActiveScreen"
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
        void $ runEffectFn5 JB.uploadMultiPartData url (EndPoint.updateSosMedia state.data.sosId) "Audio" "fileUrl" "payload"
      Nothing -> pure unit
    pure NoAction
  ]

eval (UploadMultiPartDataCallback _ fileId) state = do
  void $ pure $ JB.toggleBtnLoader "" false
  if DS.null fileId then do
    void $ pure $ EHU.showToast "Failed to upload media file. Please try again."
    continue state 
  else do
    void $ pure $ EHU.showToast "Audio shared successfully"
    continue state {props { recordedAudioUrl = Nothing, audioRecordingStatus = CTA.NOT_RECORDING, recordingTimer = "00 : 00", isAudioRecordingActive = false } }
eval (SafetyAudioRecordingAction SafetyAudioRecording.CancelAudioRecording) state = 
  continueWithCmd state { props { audioRecordingStatus = CTA.NOT_RECORDING, recordingTimer = "00 : 00", isAudioRecordingActive = false } } [do
    _   <- pure $ clearTimerWithId state.props.recordingTimerId
    void $ runEffectFn1 JB.removeMediaPlayer ""
    void $ runEffectFn1 JB.stopAudioRecording ""
    pure NoAction
  ]

eval (ShowPoliceView SafetyActionTileView.OnClick) state = continueWithCmd state { props { showCallPolice = true } } [ do
    push <- getPushFn Nothing "SosActiveScreen"
    void $ startTimer state.props.policeCallTimerValue "policeCallTimer" "1" push PoliceTimerCallback
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
  exit $ UpdateAction state "Called Safety Team"

eval OnPauseCallback state = continueWithCmd state [do
  pure $ SafetyAudioRecordingAction SafetyAudioRecording.CancelAudioRecording
]

eval (Exit output) state = exit output

eval _ state = update state

handleMediaPlayerRestart :: ST.NammaSafetyScreenState -> Eval Action ScreenOutput ST.NammaSafetyScreenState
handleMediaPlayerRestart state = continueWithCmd state { props { triggerSiren = true } }
        [ do
            push <- getPushFn Nothing "SosActiveScreen"
            void $ pure $ runFn4 JB.startAudioPlayer "ny_ic_sos_danger_full" push OnAudioCompleted "0"
            pure NoAction
        ]