module Screens.WelcomeScreen.Handler where

import Prelude (bind, ($), pure , (<$>))
import Engineering.Helpers.BackTrack (getState)
import Screens.WelcomeScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.WelcomeScreen.View as WelcomeScreen
import Types.App (FlowBT, GlobalState(..),WELCOME_SCREEN_OUTPUT(..))


welcomeScreen :: FlowBT String WELCOME_SCREEN_OUTPUT
welcomeScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ WelcomeScreen.screen state.welcomeScreen
  case act of
    MobileNumberScreen -> App.BackT $ App.NoBack <$> (pure $ GoToMobileNumberScreen)