{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.SafetyEducationScreen.Controller where

import Log (trackAppBackPress, trackAppScreenRender)
import Prelude (class Show, discard, pure, unit, void, ($), (+), (<), (==))
import PrestoDOM (class Loggable, Eval, update, continue, continueWithCmd, exit)
import Screens.Types (NammaSafetyScreenState)
import Components.GenericHeader.Controller as GenericHeaderController
import Data.Array as DA
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import JBridge (releaseYoutubeView, switchYoutubeVideo)
import Screens (ScreenName(..), getScreen)
import Screens.NammaSafetyFlow.Components.HeaderView as Header
import PrestoDOM.Types.Core (class Loggable, defaultPerformLog)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

data ScreenOutput
  = GoBack NammaSafetyScreenState
  | Refresh NammaSafetyScreenState
  | GoToHomeScreen NammaSafetyScreenState

data Action
  = BackPressed
  | NoAction
  | YoutubeVideoStatus String
  | SafetyHeaderAction Header.Action
  | ChangeEducationViewIndex Int
  | ShowVideoView Boolean

eval :: Action -> NammaSafetyScreenState -> Eval Action ScreenOutput NammaSafetyScreenState
eval (SafetyHeaderAction (Header.GenericHeaderAC GenericHeaderController.PrefixImgOnClick)) state = continueWithCmd state [ pure BackPressed ]

eval (BackPressed) state =
  if state.props.showVideoView then do
    void $ pure $ releaseYoutubeView unit
    if state.props.fromBannerLink then
      exit $ GoToHomeScreen state{data { videoList = [] }, props{ fromBannerLink = false, showVideoView = false, educationViewIndex = Nothing }}
    else continue state { props { showVideoView = false } }
  else if isNothing state.props.educationViewIndex then do
    let newState = state { data { videoList = [] }, props { fromDeepLink = false } }
    case state.props.fromDeepLink of
      true -> exit $ GoToHomeScreen newState
      false -> exit $ GoBack newState
  else do
    continue state { props { educationViewIndex = Nothing } }

eval (ChangeEducationViewIndex index) state = do
  let
    newState = state { props { educationViewIndex = Just index } }
    video = fromMaybe { videoId: "", title: "", coverImageUrl: "", description : [] } (state.data.videoList DA.!! index)
  if state.props.showVideoView then do
    void $ pure $ switchYoutubeVideo video.videoId
  else pure unit
  continue newState

eval (YoutubeVideoStatus status) state = do
  if status == "ENDED" then case state.props.educationViewIndex of
    Just index -> do
      let
        newIndex = index + 1
      if newIndex < DA.length state.data.videoList then
        continueWithCmd
          state
            { props
              { educationViewIndex = Just newIndex
              }
            }
          [ pure $ ChangeEducationViewIndex newIndex ]
      else
        continueWithCmd state [ pure BackPressed ]
    Nothing -> continue state
  else
    continue state

eval (ShowVideoView val) state = continue state { props { showVideoView = val } }

eval _ state = update state
