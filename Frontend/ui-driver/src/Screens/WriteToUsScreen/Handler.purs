module Screens.WriteToUsScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>))
import Screens.WriteToUsScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.WriteToUsScreen.View as WriteToUsScreen
import Types.App (GlobalState(..), FlowBT, WRITE_TO_US_SCREEN_OUTPUT(..))

writeToUsScreen :: FlowBT String WRITE_TO_US_SCREEN_OUTPUT
writeToUsScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ WriteToUsScreen.screen state.writeToUsScreen
  case action of
    GoBack -> App.BackT $ pure App.GoBack
    GoToHomeScreen -> App.BackT $ App.BackPoint <$> (pure $ GO_TO_HOME_SCREEN_FLOW)