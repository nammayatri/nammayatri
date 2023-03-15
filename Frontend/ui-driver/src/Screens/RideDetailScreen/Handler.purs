{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideDetailScreen.Handler where

import Prelude (bind, pure, ($), (<$>), discard)
import Engineering.Helpers.BackTrack (getState)
import Screens.RideDetailScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.RideDetailScreen.View as RideDetailScreen
import Types.App (FlowBT, GlobalState(..), RIDE_DETAIL_SCREENOUTPUT(..), ScreenType(..), defaultGlobalState)
import Types.ModifyScreenState (modifyScreenState)

rideDetail :: FlowBT String RIDE_DETAIL_SCREENOUTPUT
rideDetail = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ RideDetailScreen.screen state.rideDetailScreen
  case action of
    GoBack -> App.BackT $ pure App.GoBack
    GoToHomeScreen -> do
      let (GlobalState defaultState) = defaultGlobalState
      modifyScreenState $ HomeScreenStateType (\homeScreen → defaultState.homeScreen)
      App.BackT $ App.BackPoint <$> pure GO_TO_HOME_FROM_RIDE_DETAIL
    ShowRoute -> App.BackT $ App.BackPoint <$> pure SHOW_ROUTE_IN_RIDE_DETAIL