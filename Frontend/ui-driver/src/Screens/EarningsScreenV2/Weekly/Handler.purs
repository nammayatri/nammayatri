module Screens.EarningsScreen.Weekly.Handler where

import Prelude
import PrestoDOM.Core.Types.Language.Flow
import Screens.EarningsScreen.Weekly.View
import Screens.EarningsScreen.ScreenData
import Prelude
import Screens.EarningsScreen.ScreenData
import Types.App
import Presto.Core.Types.Language.Flow

earningScreenWeeklyV2 :: Flow GlobalState FlowState
earningScreenWeeklyV2 = do
  out <- runScreen $ screen initialState
  case out of
    _ -> pure $ EarningsV2Daily