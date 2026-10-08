module Screens.WelcomeScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Engineering.Helpers.Commons (markPerformance)
import Prelude
import PrestoDOM.Core.Types.Language.Flow (runLoggableScreen)
import Screens.WelcomeScreen.Controller (ScreenOutput(..))
import Screens.WelcomeScreen.View as WelcomeScreen
import Types.App (FlowBT, GlobalState(..), WELCOME_SCREEN_OUTPUT(..))


welcomeScreen :: FlowBT String WELCOME_SCREEN_OUTPUT
welcomeScreen = do 
  (GlobalState state) <- getState
  liftFlowBT $ markPerformance "WELCOME_SCREEN"
  act <- lift $ lift $ runLoggableScreen $ WelcomeScreen.screen state.welcomeScreen
  case act of
    MobileNumberScreen -> App.BackT $ App.BackPoint <$> (pure $ GoToMobileNumberScreen)