{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.SosActiveScreen.Controller where

import Data.Maybe
import Log
import Prelude
import PrestoDOM
import Screens.Types
import Storage
import Components.GenericHeader.Controller as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Data.Array as DA
import Data.String as DS
import Engineering.Helpers.Commons as EHC
import Helpers.Utils as HU
import Screens.NammaSafetyFlow.Components.SafetyUtils (getDefaultPriorityList)
import JBridge (askRequestedPermissions, showDialer)
import Language.Strings (getString)
import Language.Types (STR(..))
import Presto.Core.Types.Language.Flow (delay)
import PrestoDOM.Core (getPushFn)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Screens.NammaSafetyFlow.Components.HeaderView as Header
import Services.API (ContactDetails(..), GetEmergencySettingsRes(..))
import Services.Config (getSupportNumber)
import Types.App (defaultGlobalState)
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
  | UpdateAsSafe NammaSafetyScreenState
  | GoToEducationScreen NammaSafetyScreenState

data Action
  = BackPressed
  | NoAction
  | SafetyHeaderAction Header.Action
  | UpdateEmergencySettings GetEmergencySettingsRes
  | DisableShimmer
  | MarkRideAsSafe PrimaryButtonController.Action
  | CallContact Int
  | CallPolice
  | ShowPoliceView
  | LearnMoreClicked
  | PlaceCall
  | SelectedCurrentLocation Number Number String
  | ContactCircleAction ContactCircle.Action

eval :: Action -> NammaSafetyScreenState -> Eval Action ScreenOutput NammaSafetyScreenState
eval PlaceCall state = do
  let
    _ = spy "NammaSafetyFlow.SosActiveScreen.Controller" state
  let primaryContact = DA.filter (\item -> item.priority == 0) state.data.emergencyContactsList
  case primaryContact DA.!! 0, state.props.shouldCallAutomatically, state.data.shareToEmergencyContacts of
    Just contact, true, true -> void $ pure $ showDialer contact.number true
    _, _, _ -> pure unit
  continue
    state
      { props
        { shouldCallAutomatically = false
        }
      }

eval (SafetyHeaderAction (Header.GenericHeaderAC GenericHeaderController.PrefixImgOnClick)) state = continueWithCmd state [ pure BackPressed ]

eval (BackPressed) state =
  if state.props.showCallPolice then
    continue state { props { showCallPolice = false } }
  else
    exit $ GoBack state

eval DisableShimmer state = continue state { props { showShimmer = false } }

eval (CallContact contactIndex) state = do
  case state.data.emergencyContactsList DA.!! contactIndex of
    Just item -> void $ pure $ showDialer item.number true
    Nothing -> pure unit
  continue state

eval ShowPoliceView state = continue state { props { showCallPolice = true } }

eval CallPolice state = do
  void $ pure $ showDialer "112" false
  continue state

eval (MarkRideAsSafe PrimaryButtonController.OnClick) state = exit $ UpdateAsSafe state

eval LearnMoreClicked state = exit $ GoToEducationScreen state

eval (SelectedCurrentLocation lat lon name) state = continue state { data { currentLocation = name } }

eval (_) state = continue state