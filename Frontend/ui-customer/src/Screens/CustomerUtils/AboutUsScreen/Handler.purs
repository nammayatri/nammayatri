module Screens.AboutUsScreen.Handler where

import Prelude (Unit, bind, pure, ($), (<$>))
import Engineering.Helpers.BackTrack (getState)
import Screens.AboutUsScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.AboutUsScreen.View as AboutUsScreen
import Types.App (FlowBT, GlobalState(..), ABOUT_US_SCREEN_OUTPUT(..))


aboutUsScreen :: FlowBT String ABOUT_US_SCREEN_OUTPUT
aboutUsScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ AboutUsScreen.screen state.aboutUsScreen
  case action of
    GoToHomeScreen -> App.BackT $ App.BackPoint <$> (pure $ GO_TO_HOME_FROM_ABOUT)
