module Screens.MetroWarriorsScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Engineering.Helpers.Commons (markPerformance)
import Prelude
import PrestoDOM.Core.Types.Language.Flow (runLoggableScreen)
import Screens.MetroWarriorsScreen.Controller (ScreenOutput(..))
import Screens.MetroWarriorsScreen.View as MetroWarriorsScreen
import Types.App (FlowBT, GlobalState(..), METRO_WARRIOR_SCREEN_OUTPUT(..))


metroWarriorsScreen :: FlowBT String METRO_WARRIOR_SCREEN_OUTPUT
metroWarriorsScreen = do 
  (GlobalState state') <- getState
  act <- lift $ lift $ runLoggableScreen $ MetroWarriorsScreen.screen state'.metroWarriorsScreen
  case act of
    GoToHomeScreen state -> App.BackT $ App.BackPoint <$> (pure $ GO_TO_HOME_SCREEN_FROM_WARRIOR state)
    UpdateWarriorSettings state req -> App.BackT $ App.BackPoint <$> (pure $ UPDATE_WARRIOR_SETTINGS state req)