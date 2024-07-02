module Screens.WelcomeScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Engineering.Helpers.Commons (markPerformance)
import Prelude
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.WelcomeScreen.Controller (ScreenOutput(..))
import Screens.WelcomeScreen.View as WelcomeScreen
import Types.App (FlowBT, GlobalState(..), WELCOME_SCREEN_OUTPUT(..))
import PrestoDOM.List as PrestoList


welcomeScreen :: FlowBT String WELCOME_SCREEN_OUTPUT
welcomeScreen = do 
  (GlobalState state) <- getState
  liftFlowBT $ markPerformance "WELCOME_SCREEN"
  listItem <- lift $ lift $ PrestoList.preComputeListItem $ WelcomeScreen.carouselViewItem state.welcomeScreen
  act <- lift $ lift $ runScreen $ WelcomeScreen.screen state.welcomeScreen listItem
  case act of
    MobileNumberScreen -> App.BackT $ App.BackPoint <$> (pure $ GoToMobileNumberScreen)