module Screens.BankDetailsScreen.Handler where

import Prelude


import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>))
import Screens.BankDetailsScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Types.App (FlowBT, GlobalState(..), BANK_DETAILS_SCREENOUTPUT(..))
import  Screens.BankDetailsScreen.View as BankDetailScreen


bankDetailsScreen :: FlowBT String BANK_DETAILS_SCREENOUTPUT
bankDetailsScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ BankDetailScreen.screen state.bankDetailsScreen
  case action of
    _ -> App.BackT $ pure App.GoBack
