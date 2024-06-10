module Screens.DeleteAccountScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>))
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.DeleteAccountScreen.Controller (ScreenOutput(..)) as SOut
import Screens.DeleteAccountScreen.View as DeleteAccountScreen
import Types.App (DeleteAccountScreenOutput(..), FlowBT, GlobalState(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)

deleteAccountScreen :: FlowBT String DeleteAccountScreenOutput
deleteAccountScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ DeleteAccountScreen.screen state.deleteAccountScreen
  case action of
    SOut.Back _ -> App.BackT $ App.NoBack <$> pure Back
    SOut.Submit _ -> App.BackT $ App.NoBack <$> pure SubmitRequest
