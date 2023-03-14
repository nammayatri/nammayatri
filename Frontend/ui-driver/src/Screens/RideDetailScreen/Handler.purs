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