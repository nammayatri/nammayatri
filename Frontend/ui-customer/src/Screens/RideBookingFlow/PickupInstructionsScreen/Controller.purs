{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideBookingFlow.PickupInstructionsScreen.Controller where

import Prelude
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Components.PrimaryButton as PrimaryButton
import Common.Types.App as CTA
import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import JBridge as JB
import Screens.Types as ST

data ScreenOutput = NextScreen ST.PickupInstructionsScreenState | Back ST.PickupInstructionsScreenState

data Action = 
    NextClick 
    | BackClick
    | PrimaryButtonAC PrimaryButton.Action

instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = pure unit

eval :: Action -> ST.PickupInstructionsScreenState -> Eval Action ScreenOutput ST.PickupInstructionsScreenState
eval NextClick state = exit $ NextScreen state
eval BackClick state = exit $ Back state
eval (PrimaryButtonAC PrimaryButton.OnClick) state = exit $ Back state
eval _ state = update state