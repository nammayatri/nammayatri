module Screens.EarningsScreen.Daily.Handler where

import Prelude
import PrestoDOM.Core.Types.Language.Flow
import Screens.EarningsScreen.Daily.View
import Screens.EarningsScreen.ScreenData
import Prelude
import PrestoDOM.Core.Types.Language.Flow
import Screens.EarningsScreen.ScreenData
import Prelude
import Screens.EarningsScreen.ScreenData
import Types.App
import Presto.Core.Types.Language.Flow
import Screens.EarningsScreen.Controller
import Types.ModifyScreenState

earningScreenDailyV2 :: Flow GlobalState FlowState
earningScreenDailyV2 = do
  (GlobalState state) <- getState
  out <- runScreen $ screen state.earningsScreenV2
  case out of
    GoToWeekly updatedState -> do
        void $ modifyScreenStateFlow $ EarningsScreenV2 (\_ -> updatedState)
        pure $ EarningsV2Weekly
    _ -> pure $ EarningsV2Daily