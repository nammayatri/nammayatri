module Screens.RideSummaryScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.RideSummaryScreen.ScreenData
import Screens.RideSummaryScreen.View as RideSummaryScreen
import Types.App (RIDE_SUMMARY_SCREEN_OUTPUT(..), GlobalState(..), FlowBT)
import Screens.RideSummaryScreen.Controller

rideSummaryScreen :: FlowBT String RIDE_SUMMARY_SCREEN_OUTPUT
rideSummaryScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ RideSummaryScreen.screen state.rideSummaryScreen
  case act of
    (AcceptScheduleRide bookingId)-> App.BackT $ App.BackPoint <$> pure (ACCEPT_SCHEDULED_RIDE bookingId)
    GoBack -> App.BackT $ App.BackPoint <$> pure (GO_TO_RIDE_REQUEST)
    (UpdateRouteInterCity slat slon dlat dlon)  -> App.BackT $ App.BackPoint <$> pure (UPDATE_ROUTE_INTERCITY slat slon dlat dlon)
    (UpdateRouteRental lat lon)  -> App.BackT $ App.BackPoint <$> pure (UPDATE_ROUTE_RENTAL lat lon)
    (UpdateRouteRegular slat slon dlat dlon)  -> App.BackT $ App.BackPoint <$> pure (UPDATE_ROUTE_REGULAR slat slon dlat dlon)


  