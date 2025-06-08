{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.DriverEarningsScreenV2.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Data.Maybe (Maybe(..))
import Engineering.Helpers.BackTrack (getState)
import Engineering.Helpers.Commons (liftFlow)
import Prelude (bind, pure, ($), (<$>), discard)
import PrestoDOM.Core (getPushFn)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.DriverEarningsScreenV2.Controller (ScreenOutput(..))
import Screens.DriverEarningsScreen.ScreenData (initData) as DriverEarningsScreenData
import Screens.DriverEarningsScreenV2.View as DriverEarningsScreenV2
import Types.App (FlowBT, GlobalState(..), DRIVER_EARNINGS_SCREEN_V2_OUTPUT(..), NAVIGATION_ACTIONS(..), ScreenType(..), NAVIGATION_ACTIONS(..))
import Types.ModifyScreenState (modifyScreenState)

driverEarningsScreenV2 :: FlowBT String DRIVER_EARNINGS_SCREEN_V2_OUTPUT
driverEarningsScreenV2 = do
  (GlobalState state) <- getState
  push <- lift $ lift $ liftFlow $ getPushFn Nothing "DriverEarningsScreenV2"
  act <- lift $ lift $ runScreen $ DriverEarningsScreenV2.screen state.driverEarningsScreen
  case act of
    GoBack -> App.BackT $ pure App.GoBack
    HomeScreen updatedState -> do
      modifyScreenState $ DriverEarningsScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ EARNINGS_NAV_V2 HomeScreenNav updatedState)
    SubscriptionScreen updatedState -> do
      modifyScreenState $ DriverEarningsScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ EARNINGS_NAV_V2 GoToSubscription updatedState)
    Contest updatedState -> do
      modifyScreenState $ DriverEarningsScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ EARNINGS_NAV_V2 GoToContest updatedState)
    Alerts updatedState -> do
      modifyScreenState $ DriverEarningsScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ EARNINGS_NAV_V2 GoToAlerts updatedState)
    ChangeDriverEarningsTab subView updatedState -> do
      modifyScreenState $ DriverEarningsScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ CHANGE_SUB_VIEW_V2 subView updatedState)
    RefreshScreen updatedState -> do
      modifyScreenState $ DriverEarningsScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ REFRESH_EARNINGS_SCREEN_V2 updatedState)

