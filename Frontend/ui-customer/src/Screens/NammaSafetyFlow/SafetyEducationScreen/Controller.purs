{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.SafetyEducationScreen.Controller where

import Log
import Prelude
import PrestoDOM
import Screens.Types
import Storage
import Components.GenericHeader.Controller as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Data.Array as DA
import Data.Maybe
import Data.String as DS
import Engineering.Helpers.Commons as EHC
import Helpers.Utils as HU
import JBridge (askRequestedPermissions, releaseYoutubeView, switchYoutubeVideo)
import Language.Strings (getString)
import Language.Types (STR(..))
import Presto.Core.Types.Language.Flow (delay)
import PrestoDOM.Core (getPushFn)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.NammaSafetyFlow.Components.HeaderView as Header
import Services.API (ContactDetails(..), GetEmergencySettingsRes(..))
import Services.Config (getSupportNumber)
import Types.App (defaultGlobalState)
import Screens.NammaSafetyFlow.Components.SafetyUtils (getDefaultPriorityList)
import Types.EndPoint (updateSosVideo)
import Debug

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    BackPressed -> trackAppBackPress appId (getScreen NAMMASAFETY_SCREEN)
    _ -> trackAppScreenRender appId "screen" (getScreen NAMMASAFETY_SCREEN)

data ScreenOutput
  = GoBack NammaSafetyScreenState
  | PostEmergencySettings NammaSafetyScreenState
  | GoToEmergencyContactScreen NammaSafetyScreenState
  | Refresh NammaSafetyScreenState

data Action
  = BackPressed
  | NoAction
  | YoutubeVideoStatus String
  | SafetyHeaderAction Header.Action
  | ChangeEducationViewIndex Int

eval :: Action -> NammaSafetyScreenState -> Eval Action ScreenOutput NammaSafetyScreenState
eval (SafetyHeaderAction (Header.GenericHeaderAC GenericHeaderController.PrefixImgOnClick)) state = continueWithCmd state [ pure BackPressed ]

eval (BackPressed) state = 
  if isNothing state.props.educationViewIndex then
    exit $ GoBack state
  else do
    void $ pure $ releaseYoutubeView unit
    continue state { props { educationViewIndex = Nothing } }

eval (ChangeEducationViewIndex index) state = do
  let
    newState = state { props { educationViewIndex = Just index } }
    video = fromMaybe { videoId: "", title: "", coverImageUrl: "" } (state.data.videoList DA.!! index)
  void $ pure $ switchYoutubeVideo video.videoId
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
        continue state { props { educationViewIndex = Nothing } }
    Nothing -> continue state
  else
    continue state

eval _ state = continue state
