module Screens.RideSummaryScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude
-- import Screens.RideSummaryScreen.Controller
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.RideSummaryScreen.ScreenData
import Screens.RideSummaryScreen.View as RideSummaryScreen
import Types.App (RIDE_SUMMARY_SCREEN_OUTPUT(..), GlobalState(..), FlowBT)
import Engineering.Helpers.BackTrack (liftFlowBT)
import PrestoDOM.Core (getPushFn)
import Debug
import Data.Maybe (Maybe(..))
import Screens.RideSummaryScreen.Controller

rideSummaryScreen :: FlowBT String RIDE_SUMMARY_SCREEN_OUTPUT
rideSummaryScreen = do
  (GlobalState state) <- getState
  push <- liftFlowBT $ getPushFn Nothing "e7148c33-7bdd-4073-bbd3-e273ef4b2cb1"
  act <- lift $ lift $ runScreen $ RideSummaryScreen.screen initData
  case act of
    AcceptScheduleRide -> App.BackT $ App.BackPoint <$> pure (ACCEPT_SCHEDULED_RIDE "ayush")
    GoBack -> App.BackT $ App.BackPoint <$> pure (GO_TO_HOME_FROM_RIDE_SUMMARY)


  