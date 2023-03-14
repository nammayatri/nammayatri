module Screens.AccountSetUpScreen.Handler where

import Prelude (bind, ($), pure , (<$>))
import Engineering.Helpers.BackTrack (getState)
import Screens.AccountSetUpScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.AccountSetUpScreen.View as AccountSetUpScreen
import Types.App (FlowBT, GlobalState(..),ACCOUNT_SET_UP_SCREEN_OUTPUT(..))


accountSetUpScreen ::FlowBT String ACCOUNT_SET_UP_SCREEN_OUTPUT
accountSetUpScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ AccountSetUpScreen.screen state.accountSetUpScreen
  case act of
    GoHome updatedState ->  App.BackT $ App.NoBack <$> (pure $ GO_HOME updatedState)
    ChangeMobileNumber -> App.BackT $ App.NoBack <$> (pure $ GO_BACK)