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
import Prelude (class Show, discard, pure, unit, void, ($), (==))
import PrestoDOM (Eval, continue, continueWithCmd, exit)
import Screens.Types (NammaSafetyScreenState)
import Components.GenericHeader.Controller as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Data.Array as DA
import JBridge (showDialer)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.NammaSafetyFlow.Components.ContactCircle as ContactCircle
import Screens.NammaSafetyFlow.Components.HeaderView as Header
import Services.API (GetEmergencySettingsRes)

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
  | UpdateAction NammaSafetyScreenState String

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
    primaryContact = DA.filter (\item -> item.priority == 0) state.data.emergencyContactsList
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

eval (SafetyHeaderAction (Header.BackClicked)) state = continueWithCmd state [ pure BackPressed ]

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
  if state.props.showTestDrill then
    continue state
  else
    exit $ UpdateAction state "Called Emergency Contact"

eval ShowPoliceView state = continue state { props { showCallPolice = true } }

eval CallPolice state = do
  void $ pure $ showDialer "112" false
  exit $ UpdateAction state "Called Police"

eval (MarkRideAsSafe PrimaryButtonController.OnClick) state = exit $ UpdateAsSafe state

eval LearnMoreClicked state = exit $ GoToEducationScreen state

eval (SelectedCurrentLocation _ _ name) state = continue state { data { currentLocation = name } }

eval _ state = continue state
