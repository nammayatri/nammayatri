module Screens.SelectLanguageScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>))
import Screens.SelectLanguageScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.SelectLanguageScreen.View as SelectLanguageScreen
import Types.App (GlobalState(..), FlowBT, SELECT_LANGUAGE_SCREEN_OUTPUT(..))

selectLanguageScreen :: FlowBT String SELECT_LANGUAGE_SCREEN_OUTPUT
selectLanguageScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ SelectLanguageScreen.screen state.selectedLanguageScreen
  case action of
    GoBack -> App.BackT $ App.NoBack <$> pure CHANGE_LANGUAGE