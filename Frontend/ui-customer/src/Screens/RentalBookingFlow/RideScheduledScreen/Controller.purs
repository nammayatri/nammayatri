{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RentalBookingFlow.RideScheduledScreen.Controller where

import Components.GenericHeader.Controller as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.SourceToDestination.Controller as SourceToDestinationActionController
import Data.Maybe (Maybe)
import Log (trackAppActionClick, trackAppEndScreen)
import Prelude (class Show, discard, pure, unit)
import PrestoDOM (class Loggable, Eval, continue, exit)
import Screens (getScreen, ScreenName(..))
import Screens.Types (RideScheduledScreenState)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    PrimaryButtonActionController _-> trackAppActionClick appId (getScreen RIDE_SCHEDULED_SCREEN) "primary_button" "on_click"
    SourceToDestinationAC _ -> trackAppActionClick appId (getScreen RIDE_SCHEDULED_SCREEN) "source_to_destination" "dummy_action"
    CancelRide -> trackAppActionClick appId (getScreen RIDE_SCHEDULED_SCREEN) "cancel_ride" "on_click"
    AddFirstStop _ -> do 
      trackAppActionClick appId (getScreen RIDE_SCHEDULED_SCREEN) "add_first_stop" "on_click"
      trackAppEndScreen appId (getScreen RIDE_SCHEDULED_SCREEN)
    _ -> pure unit

data Action
  = NoAction
  | PrimaryButtonActionController PrimaryButtonController.Action
  | SourceToDestinationAC SourceToDestinationActionController.Action
  | CancelRide
  | AddFirstStop (Maybe String)
  | GenericHeaderAC GenericHeaderController.Action

data ScreenOutput = GoToHomeScreen | GoToSearchLocationScreen

eval :: Action -> RideScheduledScreenState -> Eval Action ScreenOutput RideScheduledScreenState
eval (PrimaryButtonActionController (PrimaryButtonController.OnClick)) state = exit GoToHomeScreen
eval (AddFirstStop _) _ = exit GoToSearchLocationScreen
eval _ state = continue state
