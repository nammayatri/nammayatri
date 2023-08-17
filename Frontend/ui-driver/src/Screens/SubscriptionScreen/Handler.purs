module Screens.SubscriptionScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.SubscriptionScreen.Controller (ScreenOutput(..))
import Screens.SubscriptionScreen.View as SubscriptionScreen
import Types.App (FlowBT, GlobalState(..), SUBSCRIPTION_SCREEN_OUTPUT(..), ScreenType(..), NAVIGATION_ACTIONS(..))
import Types.ModifyScreenState (modifyScreenState)


subscriptionScreen :: FlowBT String SUBSCRIPTION_SCREEN_OUTPUT
subscriptionScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ SubscriptionScreen.screen state.subscriptionScreen (GlobalState state)
  case act of
    HomeScreen updatedState -> do 
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ NAV HomeScreenNav)
    RideHistory updatedState -> do 
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ NAV GoToRideHistory)
    Contest updatedState -> do 
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ NAV GoToContest)
    Alerts updatedState -> do 
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ NAV GoToAlerts)
    JoinPlanExit updatedState -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ MAKE_PAYMENT updatedState)
    PaymentHistory updatedState -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GOTO_PAYMENT_HISTORY updatedState)
    CancelAutoPayPlan updatedState -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ CANCEL_AUTOPAY updatedState)
    SwitchCurrentPlan updatedState -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ SWITCH_PLAN updatedState)
    ResumeAutoPayPlan updatedState -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ RESUME_AUTOPAY updatedState)
    CheckOrderStatus updatedState orderId -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ CHECK_ORDER_STATUS updatedState orderId)
    GotoManagePlan updatedState -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_MANAGE_PLAN updatedState)
    Refresh -> App.BackT $ App.NoBack <$> (pure $ REFRESH_SUSCRIPTION)
    RetryPayment updatedState planId -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ RETRY_PAYMENT_AC updatedState planId)