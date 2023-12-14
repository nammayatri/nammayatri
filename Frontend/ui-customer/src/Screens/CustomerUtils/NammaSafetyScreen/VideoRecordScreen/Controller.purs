{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyScreen.VideoRecordScreen.Controller where

import Debug
import Prelude
import PrestoDOM

import Components.GenericHeader.Controller as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Constants (defaultDensity)
import Data.Function.Uncurried (runFn1)
import Data.Int as DI
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (launchAff)
import Effect.Uncurried (runEffectFn5, runEffectFn7)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils (loaderText, toggleLoader, uploadMultiPartData, uploadMultiPartDataIOS)
import Helpers.Utils as HU
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (printLog, trackAppBackPress, trackAppScreenEvent)
import Presto.Core.Types.Language.Flow (delay)
import PrestoDOM.Core (getPushFn)
import PrestoDOM.Types.Core (class Loggable)
import Screens.NammaSafetyScreen.Controller (constructWhatsappMessage)
import Screens.Types (NammaSafetyScreenState, NammaSafetyStage(..), RecordingState(..))
import Types.App (defaultGlobalState)
import Types.EndPoint (updateSosVideo)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    BackPressed -> trackAppBackPress appId "NammaSafetyVideoRecordScreen"
    OnResumeCallback -> trackAppScreenEvent appId "NammaSafetyVideoRecordScreen" "OnResumeCallback" ""
    _ -> pure unit


data ScreenOutput
  = GoBack NammaSafetyScreenState

data Action
  = BackPressed
  | OnResumeCallback
  | CountDownShare Int String String String
  | VideoStatusCallBack String String
  | NoAction
  | UploadVideo
  | CountDown Int String String String
  | GenericHeaderAC GenericHeaderController.Action
  | ToggleRecord
  | VideoShared PrimaryButtonController.Action
  | ChangeRecordingState RecordingState
  | ShareSilentSos (Maybe String)
  | UploadMultiPartCallback String String

eval :: Action -> NammaSafetyScreenState -> Eval Action ScreenOutput NammaSafetyScreenState

eval OnResumeCallback state = do
  let
    shouldGoBack =
      state.props.currentStage == NammaSafetyVideoRecord
        && state.props.recordingState == UPLOADING
        && state.props.enableLocalPoliceSupport
  if shouldGoBack then do
    continueWithCmd state
      [ pure BackPressed
      ]
  else
    continue state

eval (VideoStatusCallBack status uri) state = do
  void $ pure $ spy "VideoStatusCallBack" state
  case status of
    "VIDEO_RECORDED" -> do
      if state.props.recordingState == RECORDING then
        continueWithCmd
          state
            { props
              { recordingState = getNewRecordingStage
              }
            , data { videoPath = uri }
            }
          [ pure $ if state.props.enableLocalPoliceSupport then UploadVideo else NoAction
          ]
      else
        continue state
    _ -> continue state
  where
  getNewRecordingStage = if state.props.enableLocalPoliceSupport then UPLOADING else SHARING

eval UploadVideo state =
  continueWithCmd state
    [ do
        void $ launchAff $ EHC.flowRunner defaultGlobalState
          $ do
              push <- EHC.liftFlow $ getPushFn Nothing "NammaSafetyVideoRecordScreen"
              loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
              toggleLoader true
              delay $ Milliseconds 1000.0
              if EHC.os == "IOS" then do
                void $ EHC.liftFlow $ runEffectFn7 uploadMultiPartDataIOS state.data.videoPath (updateSosVideo state.data.sosId) "Video" "video" "fileUrl" push UploadMultiPartCallback
              else do
                res <- EHC.liftFlow $ runEffectFn5 uploadMultiPartData state.data.videoPath (updateSosVideo state.data.sosId) "Video" "video" "fileUrl"
                if state.props.enableLocalPoliceSupport then do
                  EHC.liftFlow $ push $ ShareSilentSos $ Just res
                else do
                  toggleLoader false
                  EHC.liftFlow $ push
                    $ if res /= "" then
                        ChangeRecordingState SHARED
                      else
                        BackPressed
        pure $ NoAction
    ]

eval (GenericHeaderAC (GenericHeaderController.SuffixImgOnClick)) state = continueWithCmd state [ pure BackPressed ]

eval (VideoShared PrimaryButtonController.OnClick) state = 
  continueWithCmd state [ pure BackPressed ]

eval (CountDownShare seconds id status timerID) state = do
  void $ pure $ printLog "timer" $ show seconds
  if status == "EXPIRED" then do
    let
      newState = state { props { shareTimerId = "" } }
    void $ pure $ EHC.clearCountDownTimer "ShareTimer"
    if state.props.currentStage == NammaSafetyVideoRecord && state.props.recordingState == SHARING then do
      continueWithCmd newState{props{recordingState = UPLOADING}} [ pure $ UploadVideo ]
    else
      continue newState
  else
    continue state { props { shareTimerValue = seconds, shareTimerId = timerID } }

eval (CountDown seconds id status timerID) state = do
  void $ pure $ printLog "timer" $ show seconds
  if status == "EXPIRED" then do
    let
      newState = state { props { timerValue = seconds, timerId = "" } }
    void $ pure $ EHC.clearCountDownTimer "RecordingTimer"
    continueWithCmd newState [ do
      void $ JB.stopRecord unit
      pure NoAction
    ]
  else
    continue state { props { timerValue = seconds, timerId = timerID } }

eval (ToggleRecord) state =
  if state.props.recordingState == RECORDING then do
    void $ pure $ JB.stopRecord unit
    void $ pure $ EHC.clearCountDownTimer "RecordingTimer"
    continue state
  else if state.props.recordingState == SHARING then do
    continueWithCmd state [pure BackPressed]
  else do
    continue state { props { recordingState = RECORDING } }

eval (ShareSilentSos videoUri) state =
  continueWithCmd state
    [ do
        void $ JB.openUrlInApp $ constructWhatsappMessage videoUri state
        pure NoAction
    ]

eval BackPressed state = do
  void $ pure $ EHC.clearCountDownTimer <$> ["RecordingTimer", "ShareTimer"]
  exit $ GoBack state

eval (ChangeRecordingState recordingState) state = continue state { props { recordingState = recordingState } }

eval (UploadMultiPartCallback fileType response) state =
  continueWithCmd state
    [ do
        pure
          if state.props.enableLocalPoliceSupport then
            ShareSilentSos $ Just response
          else if response /= "FAILED" then ChangeRecordingState SHARED else BackPressed
    ]

eval _ state = continue state

getRecordViewWidth :: String -> Number
getRecordViewWidth dummy = requiredWidth
  where
  layoutBounds = runFn1 JB.getLayoutBounds $ EHC.getNewIDWithTag "recordProgress"
  pixels = runFn1 HU.getPixels ""
  density = (runFn1 HU.getDeviceDefaultDensity "") / defaultDensity
  requiredWidth = ((DI.toNumber layoutBounds.width) / pixels) * density
