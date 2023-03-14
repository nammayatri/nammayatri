module Screens.AboutUsScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>))
import Screens.AboutUsScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.AboutUsScreen.View as AboutUsScreen
import Types.App (ABOUT_US_SCREEN_OUTPUT(..), GlobalState(..), FlowBT)

aboutUsScreen :: FlowBT String ABOUT_US_SCREEN_OUTPUT
aboutUsScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ AboutUsScreen.screen state.aboutUsScreen
  case action of
    GoBack -> App.BackT $ pure App.GoBack
    GoToHome updatedState -> App.BackT $ App.NoBack <$> (pure $ GO_TO_DRIVER_HOME_SCREEN)