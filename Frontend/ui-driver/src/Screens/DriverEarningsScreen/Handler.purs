{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverEarningsScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Data.Maybe (Maybe(..))
import Engineering.Helpers.BackTrack (getState)
import Engineering.Helpers.Commons (liftFlow)
import Prelude (bind, pure, ($), (<$>), discard)
import PrestoDOM.Core (getPushFn)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.DriverEarningsScreen.Controller (ScreenOutput(..))
import Screens.DriverEarningsScreen.ScreenData (initData) as DriverEarningsScreenData
import Screens.DriverEarningsScreen.View as DriverEarningsScreen
import Types.App (FlowBT, GlobalState(..), DRIVER_EARNINGS_SCREEN_OUTPUT(..), NAVIGATION_ACTIONS(..), ScreenType(..), NAVIGATION_ACTIONS(..))
import Types.ModifyScreenState (modifyScreenState)

driverEarningsScreen :: FlowBT String DRIVER_EARNINGS_SCREEN_OUTPUT
driverEarningsScreen = do
  (GlobalState state) <- getState
  push <- lift $ lift $ liftFlow $ getPushFn Nothing "DriverEarningsScreen"
  act <- lift $ lift $ runScreen $ DriverEarningsScreen.screen state.driverEarningsScreen
  case act of
    GoBack -> App.BackT $ pure App.GoBack
    HomeScreen updatedState -> do 
      modifyScreenState $ DriverEarningsScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ EARNINGS_NAV HomeScreenNav)
    SubscriptionScreen updatedState -> do 
      modifyScreenState $ DriverEarningsScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ EARNINGS_NAV GoToSubscription)
    Contest updatedState -> do 
      modifyScreenState $ DriverEarningsScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ EARNINGS_NAV GoToContest)
    Alerts updatedState -> do 
      modifyScreenState $ DriverEarningsScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ EARNINGS_NAV GoToAlerts)
    ChangeDriverEarningsTab subView -> do 
      -- modifyScreenState $ DriverEarningsScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ CHANGE_SUB_VIEW subView)
    PurchasePlan updatedState -> do 
      modifyScreenState $ DriverEarningsScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ CONVERT_COIN_TO_CASH updatedState)