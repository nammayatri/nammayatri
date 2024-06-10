{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.Benefits.LmsVideoScreen.Controller where

import Prelude
import Screens.Types as ST
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Log (trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import PrestoDOM.Core (getPushFn)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect.Uncurried (runEffectFn5)
import JBridge as JB
import RemoteConfig as RC
import Components.GenericHeader as GenericHeader
import Screens.Benefits.LmsVideoScreen.Transformer (transformReelsPurescriptDataToNativeData, buildLmsVideoeRes)
import Engineering.Helpers.Commons (getNewIDWithTag, flowRunner, jBridgeMethodExists)
import Debug (spy)
import Foreign.Generic (encodeJSON)
import Foreign (Foreign)
import DecodeUtil (decodeForeignObject, decodeForeignAny)
import Effect.Aff (launchAff)
import Types.App (defaultGlobalState)
import Services.Backend as Remote
import Services.API as API
import Services.Accessor (_moduleId)
import Locale.Utils
import Data.Lens ((^.))
import Data.Array (find, null)
import Storage(setValueToLocalStore, KeyStore(..))
import Effect.Class (liftEffect)
import Control.Monad.Except.Trans (lift)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    _ -> trackAppScreenEvent appId (getScreen LMS_VIDEO_SCREEN) "in_screen" "no_action"

data Action = NoAction
            | GenericHeaderActionController GenericHeader.Action
            | BackPressed
            | TakeQuiz
            | OpenReelsView Int String API.LmsVideoRes
            | GetCurrentPosition String String Foreign Foreign
            | UpdateVideoList API.LmsGetVideosRes
            | ErrorOccuredAction
            | SelectLanguage 
            | YoutubeVideoStatus String

data ScreenOutput = GoBack ST.LmsVideoScreenState
                  | GoToTakeQuiz ST.LmsVideoScreenState
                  | RefreshLmsVideoScreen ST.LmsVideoScreenState
                  | GoToBenefitsScreen ST.LmsVideoScreenState
                  | GoToSelectLanguage ST.LmsVideoScreenState

eval :: Action -> ST.LmsVideoScreenState -> Eval Action ScreenOutput ST.LmsVideoScreenState

eval (GenericHeaderActionController (GenericHeader.PrefixImgOnClick)) state = exit $ GoToBenefitsScreen state

eval BackPressed state = if state.data.ytVideo.enable then do
  void $ pure $ JB.releaseYoutubeView unit
  continue state{data{ytVideo {enable = false}}} 
  else exit $ GoToBenefitsScreen state

eval TakeQuiz state = exit $ GoToTakeQuiz state {props {isFetchingQuiz = true}}

eval (OpenReelsView index videoStatus (API.LmsVideoRes video)) state = do
  if jBridgeMethodExists "addReels" then do
    void $ pure $ setValueToLocalStore ANOTHER_ACTIVITY_LAUNCHED "true"
    continueWithCmd state{props{showShimmer = true}} [ do
      push <-  getPushFn Nothing "LmsVideoScreen"
      void $ runEffectFn5 JB.addReels (encodeJSON (transformReelsPurescriptDataToNativeData $ if videoStatus == "PENDING" then state.data.videosScreenData.pending else state.data.videosScreenData.completed)) index (getNewIDWithTag "LmsYoutubeView") push $ GetCurrentPosition
      pure NoAction
    ]
  else continue state{data{ytVideo {videoId = "w8rH5IjJvg0", enable = true}}}

eval (UpdateVideoList res) state = let generatedData = buildLmsVideoeRes res in continue state {props {showShimmer = false, showError = null generatedData.pending && null generatedData.completed}, data {videosScreenData = buildLmsVideoeRes res}}

eval (GetCurrentPosition label stringData reelData buttonData) state =
  let currentReelVideoData = decodeForeignAny reelData defaultReelItem
      videoId = maybe "" (\rData -> rData.id) currentReelVideoData
      currentButtonConfig = decodeForeignAny buttonData RC.defaultReelButtonConfig
      shareMessageTitle = maybe Nothing (\rButtonData -> rButtonData.shareMessageTitle) currentButtonConfig
      shareText = maybe Nothing (\rButtonData -> rButtonData.shareText) currentButtonConfig
      shareLink = maybe Nothing (\rButtonData -> rButtonData.shareLink) currentButtonConfig
  in
  case label of
    "ACTION" -> 
      case stringData of
        "DESTROY_REEL" -> do
           void $ pure $ setValueToLocalStore ANOTHER_ACTIVITY_LAUNCHED "false"
           updateAndExit state {props {showShimmer = true}} $ RefreshLmsVideoScreen state {props {showShimmer = true}}
        "SHARE" -> do 
            _ <- pure $ JB.shareTextMessage (fromMaybe "" shareMessageTitle) (fromMaybe "" shareText)
            continue state {props {showShimmer = true}}
        "OPEN_LINK" -> do
            _ <- pure $ JB.openUrlInApp (fromMaybe "www.nammayatri.in" shareLink)
            continue state
        _ -> continue state {props{showShimmer = true}}
      
    "CURRENT_POSITION" -> continue state{props{showShimmer = true}}
    "CURRENT_VIDEO_PERCENTAGE_COMPLETED" -> continue state
    "CURRENT_VIDEO_START_THRESHOLD_CROSSED" ->
        case (find (\(API.LmsVideoRes video) -> video.videoId == videoId) (state.data.videosScreenData.completed <> state.data.videosScreenData.pending) )of
          Nothing -> continue state
          Just (API.LmsVideoRes video) ->
            continueWithCmd state
            [ do
                void $ launchAff $ flowRunner defaultGlobalState
                  $ do
                      _ <- Remote.markVideoAsStarted (API.StartVideoUpdateAPIReq{moduleId : video.moduleId , videoId : video.videoId, language: video.language})
                      pure unit
                pure NoAction
            ]
    
    a | a == "CURRENT_VIDEO_END_THRESHOLD_CROSSED" || a == "CURRENT_VIDEO_COMPLETED_100" ->
        case (find (\(API.LmsVideoRes video) -> video.videoId == videoId) (state.data.videosScreenData.completed <> state.data.videosScreenData.pending) )of
          Nothing -> continue state
          Just (API.LmsVideoRes video) ->
            continueWithCmd state
            [ do
                void $ launchAff $ flowRunner defaultGlobalState
                  $ do
                      _ <- Remote.markVideoAsCompleted (API.CompletedVideoUpdateAPIReq{moduleId : video.moduleId, videoId : video.videoId, language: video.language})
                      pure unit
                pure NoAction
            ]

    _ -> continue state

eval ErrorOccuredAction state = continue state {props{showError = true, showShimmer = false}}

eval SelectLanguage state = exit $ GoToSelectLanguage state

eval _ state = update state

defaultReelItem :: Maybe RC.ReelItem
defaultReelItem = Nothing