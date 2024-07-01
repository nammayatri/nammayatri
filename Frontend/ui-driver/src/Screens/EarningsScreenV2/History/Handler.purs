module Screens.EarningsScreen.History.Handler where

import Prelude
import PrestoDOM.Core.Types.Language.Flow
import Screens.EarningsScreen.History.View
import Screens.EarningsScreen.ScreenData
import Types.App
import Presto.Core.Types.Language.Flow

earningsHistoryFlow :: HistoryScreen -> Flow GlobalState FlowState
earningsHistoryFlow screenType = do
  out <- runScreen $ screen initialState screenType
  case out of
    _ -> pure $ EarningsV2Weekly