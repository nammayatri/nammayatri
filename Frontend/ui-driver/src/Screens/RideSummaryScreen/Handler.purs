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

rideSummaryScreen :: FlowBT String ScreenOutput
rideSummaryScreen = do
  (GlobalState state) <- getState
  push <- liftFlowBT $ getPushFn Nothing "RideSummaryScreen"
  lift $ lift $ runScreen $ RideSummaryScreen.screen initData
  -- case act of
  --   GoBack -> App.BackT $ App.BackPoint <$> pure (GOTO_HOME)

  