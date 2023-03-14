module Screens.InvoiceScreen.Handler where

import Prelude (Unit, bind, ($), (<$>), pure)
import Engineering.Helpers.BackTrack (getState)
import Screens.InvoiceScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.InvoiceScreen.View as InvoiceScreen
import Types.App (FlowBT, GlobalState(..))

invoiceScreen :: FlowBT String ScreenOutput
invoiceScreen = do 
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ InvoiceScreen.screen state.invoiceScreen
  case act of
    GoBack -> App.BackT $ App.NoBack <$> (pure $ act) 
    GoToHome -> App.BackT  $ App.NoBack <$> (pure $ act)