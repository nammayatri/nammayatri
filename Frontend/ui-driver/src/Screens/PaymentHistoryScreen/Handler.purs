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
    ViewPaymentDetails updatedState -> do 
      modifyScreenState $ PaymentHistoryScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ ViewDetails updatedState)
    GoBack -> do
      -- modifyScreenState $ PaymentHistoryScreenStateType (\_ -> updatedState)
      App.BackT $ pure App.GoBack