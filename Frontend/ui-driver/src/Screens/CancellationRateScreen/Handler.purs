module Screens.CancellationRateScreen.Handler where

import Prelude
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>))
import Screens.CancellationRateScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runLoggableScreen)
import Screens.CancellationRateScreen.View as CancellationRateScreen
import Types.App (GlobalState(..), FlowBT)

cancellationRateScreen :: FlowBT String Unit
cancellationRateScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runLoggableScreen $ CancellationRateScreen.screen state.cancellationRateScreen
  void $ case action of
    GoBack -> App.BackT $ pure App.GoBack
  pure unit