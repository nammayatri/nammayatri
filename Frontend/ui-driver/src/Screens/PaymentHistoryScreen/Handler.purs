{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.PaymentHistoryScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Types.App (FlowBT, GlobalState(..), PAYMENT_HISTORY_SCREEN_OUTPUT(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)
import Screens.PaymentHistoryScreen.Controller (ScreenOutput(..))
import Screens.PaymentHistoryScreen.View as PaymentHistoryScreen

paymentHistory :: FlowBT String  PAYMENT_HISTORY_SCREEN_OUTPUT
paymentHistory = do 
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ PaymentHistoryScreen.screen state.paymentHistoryScreen
  case act of
    GoBack -> do
      App.BackT $ pure App.GoBack
    SetupAutoPay updatedState -> do
      modifyScreenState $ PaymentHistoryScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ GoToSetupAutoPay updatedState)
    ShowSummary updatedState id -> do
      modifyScreenState $ PaymentHistoryScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ EntityDetailsAPI updatedState id)
    SwitchTab updatedState -> do
      modifyScreenState $ PaymentHistoryScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ SWITCH_TAB)
    LoadMoreItems updatedState -> do
      modifyScreenState $ PaymentHistoryScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ LOAD_MORE_ITEMS updatedState)