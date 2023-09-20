{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}


module Screens.SubscriptionScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.SubscriptionScreen.Controller (ScreenOutput(..))
import Screens.SubscriptionScreen.View as SubscriptionScreen
import Screens.SubscriptionScreen.ScreenData as SubscriptionScreenData
import Types.App (FlowBT, GlobalState(..), SUBSCRIPTION_SCREEN_OUTPUT(..), ScreenType(..), NAVIGATION_ACTIONS(..))
import Types.ModifyScreenState (modifyScreenState)
import Debug

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
    SwitchCurrentPlan updatedState planId -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ SWITCH_PLAN updatedState planId)
    ResumeAutoPayPlan updatedState -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ RESUME_AUTOPAY updatedState)
    CheckOrderStatus updatedState orderId -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ CHECK_ORDER_STATUS updatedState orderId)
    GotoManagePlan updatedState -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_MANAGE_PLAN updatedState)
    GotoFindHelpCentre updatedState -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_FIND_HELP_CENTRE updatedState)
    GoToOpenGoogleMaps updatedState -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_OPEN_GOOGLE_MAPS updatedState)
    RefreshHelpCentre updatedState -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ REFRESH_HELP_CENTRE updatedState)
    Refresh -> App.BackT $ App.NoBack <$> (pure $ REFRESH_SUSCRIPTION)
    RetryPayment updatedState planId -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ RETRY_PAYMENT_AC updatedState planId)
    ClearDues updatedState -> do
      modifyScreenState $ SubscriptionScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ CLEAR_DUES_ACT)