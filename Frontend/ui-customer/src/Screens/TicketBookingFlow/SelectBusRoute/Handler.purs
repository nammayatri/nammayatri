module Screens.SelectBusRoute.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, discard, ($), (<$>), unit)
import Screens.SelectBusRoute.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import ModifyScreenState (modifyScreenState)
import Screens.SelectBusRoute.View as View
import Types.App (FlowBT, GlobalState(..), SELECT_BUS_ROUTE_SCREEN_OUTPUT(..), ScreenType(..))

selectBusRouteScreen :: String -> String -> FlowBT String SELECT_BUS_ROUTE_SCREEN_OUTPUT
selectBusRouteScreen fromStationCode toStationCode = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ View.screen fromStationCode toStationCode state.selectBusRouteScreen
  case action of
    GoBack -> App.BackT $ App.NoBack <$> (pure GO_TO_SEARCH_LOCATION_FROM_SELECT_ROUTE)
    TrackBus updatedState -> do
      App.BackT $ App.NoBack <$> (pure (TRACK_BUS updatedState))