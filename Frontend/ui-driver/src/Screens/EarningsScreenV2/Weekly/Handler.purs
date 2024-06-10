module Screens.EarningsScreen.Weekly.Handler where

import Prelude
import PrestoDOM.Core.Types.Language.Flow
import Screens.EarningsScreen.Weekly.View
import Screens.EarningsScreen.ScreenData
import Types.App
import Presto.Core.Types.Language.Flow
import Types.ModifyScreenState
import Screens.EarningsScreen.Weekly.Controller

earningScreenWeeklyV2 :: Flow GlobalState FlowState
earningScreenWeeklyV2 = do
  (GlobalState state) <- getState
  out <- runScreen $ screen state.earningsScreenV2
  case out of
    GoToHomeScreen updatedState -> do
      void $ modifyScreenStateFlow $ EarningsScreenV2 (\_ -> updatedState)
      pure $ HomeScreen
    GoToProfileScreen updatedState -> do
      void $ modifyScreenStateFlow $ EarningsScreenV2 (\_ -> updatedState)
      pure $ Profile
    GoToNotifications updatedState -> do
      void $ modifyScreenStateFlow $ EarningsScreenV2 (\_ -> updatedState)
      pure $ Notifications
    GoToReferralScreen updatedState -> do
      void $ modifyScreenStateFlow $ EarningsScreenV2 (\_ -> updatedState)
      pure $ Benefits
    SubscriptionScreen updatedState -> do
      void $ modifyScreenStateFlow $ EarningsScreenV2 (\_ -> updatedState)
      pure $ Subscription
    UpdatedWeeklyEarnings updatedState -> do
      void $ modifyScreenStateFlow $ EarningsScreenV2 (\_ -> updatedState)
      pure $ EarningsV2Weekly
    GoToRideHistoryScreen updatedState -> do
      void $ modifyScreenStateFlow $ EarningsScreenV2 (\_ -> updatedState)
      pure $ EarningsV2RideHistory
    GoToPayoutHistory updatedState -> do
      void $ modifyScreenStateFlow $ EarningsScreenV2 (\_ -> updatedState)
      pure $ EarningsV2PayoutHistory
    GoToDaily updatedState -> do
      void $ modifyScreenStateFlow $ EarningsScreenV2 (\_ -> updatedState)
      pure $ (EarningsV2Daily true)
    GoToHelpAndSupportScreen -> pure $ OpenHelpAndSuportScreen
    _ -> pure $ (EarningsV2Daily true)
