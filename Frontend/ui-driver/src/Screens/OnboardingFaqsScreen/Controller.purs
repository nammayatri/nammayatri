{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.OnboardingFaqsScreen.Controller where

import Data.Maybe
import Prelude
import PrestoDOM.Types.Core (class Loggable)
import PrestoDOM (Eval, update, continue, continueWithCmd, exit)
import Screens.Types as ST
import Helpers.Utils as HU
import JBridge as JB
import Storage (KeyStore(..), getValueToLocalStore)
import Screens.OnboardingFaqsScreen.ScreenData

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    _ -> pure unit

data Action = BackPressed

data ScreenOutput = GoBack 
                
eval :: Action -> ST.OnboardingFaqsScreenState -> Eval Action ScreenOutput ST.OnboardingFaqsScreenState

eval BackPressed state = exit $ GoBack

eval _ state = update state