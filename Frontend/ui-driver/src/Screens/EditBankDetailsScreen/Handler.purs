module Screens.EditBankDetailsScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($))
import Screens.EditBankDetailsScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.EditBankDetailsScreen.View as EditBankDetailsScreen
import Types.App (GlobalState(..), FlowBT, EDIT_BANK_DETAILS_SCREEN_OUTPUT)

editBankDetailsScreen :: FlowBT String EDIT_BANK_DETAILS_SCREEN_OUTPUT
editBankDetailsScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ EditBankDetailsScreen.screen state.editBankDetailsScreen
  case action of
    GoBack -> App.BackT $ pure App.GoBack