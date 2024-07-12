module Screens.EarningsScreen.Weekly.Handler where

import Prelude
import PrestoDOM.Core.Types.Language.Flow
import Screens.EarningsScreen.Weekly.View
import Screens.EarningsScreen.ScreenData
import Types.App
import Presto.Core.Types.Language.Flow
import Types.ModifyScreenState
import Screens.EarningsScreen.Weekly.Controller
import Debug

earningScreenWeeklyV2 :: Flow GlobalState FlowState
earningScreenWeeklyV2 = do
  (GlobalState state) <- getState
  out <- runScreen $ screen state.earningsScreenV2
  case out of
    UpdatedWeeklyEarnings updatedState -> do
      void $ pure $ spy "printing inside UpdatedWeeklyEarnings -> "  updatedState
      void $ modifyScreenStateFlow $ EarningsScreenV2 (\_ -> updatedState)
      pure $ EarningsV2Weekly
    GoToRideHistoryScreen updatedState -> do
      void $ modifyScreenStateFlow $ EarningsScreenV2 (\_ -> updatedState)
      pure $ EarningsV2RideHistory
    GoToPayoutHistory updatedState -> do
      void $ modifyScreenStateFlow $ EarningsScreenV2 (\_ -> updatedState)
      pure $ EarningsV2PayoutHistory
    _ -> pure $ EarningsV2Daily