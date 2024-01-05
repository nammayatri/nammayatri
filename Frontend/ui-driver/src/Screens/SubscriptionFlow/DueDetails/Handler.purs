{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DueDetailsScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, discard, ($), (<$>))
import Screens.DueDetailsScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.DueDetailsScreen.View as DueDetailsScreen
import Types.App (FlowBT, GlobalState(..), DUE_DETAILS_SCREEN_OUTPUT(..),ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)

dueDetailsScreen :: FlowBT String DUE_DETAILS_SCREEN_OUTPUT
dueDetailsScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ DueDetailsScreen.screen state.dueDetailsScreen
  case action of
    GoBack -> App.BackT $ pure App.GoBack
