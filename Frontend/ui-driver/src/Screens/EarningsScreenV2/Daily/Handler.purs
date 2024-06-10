module Screens.EarningsScreen.Daily.Handler where

import Prelude
import Screens.EarningsScreen.Daily.View
import Screens.EarningsScreen.ScreenData
import PrestoDOM.Core.Types.Language.Flow
import Types.App
import Presto.Core.Types.Language.Flow
import Screens.EarningsScreen.Daily.Controller
import Types.ModifyScreenState
import Effect.Aff (Milliseconds(..))

earningScreenDailyV2 :: Flow GlobalState FlowState
earningScreenDailyV2 = do
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
    GoToWeekly updatedState -> do
      void $ modifyScreenStateFlow $ EarningsScreenV2 (\_ -> updatedState)
      pure $ EarningsV2Weekly
    DateUpdated updatedState -> do 
      void $ modifyScreenStateFlow $ EarningsScreenV2 (\_ -> updatedState)
      earningScreenDailyV2
    SelectedTripDetails updatedState -> do
      void $ modifyScreenStateFlow $ EarningsScreenV2 (\_ -> updatedState)
      pure $ OpenRideDetails updatedState.data.selectedRideItem
    GoToHelpAndSupportScreen -> pure $ OpenHelpAndSuportScreen
    _ -> pure $ (EarningsV2Daily false)