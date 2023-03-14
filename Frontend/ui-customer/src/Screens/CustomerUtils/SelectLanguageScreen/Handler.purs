module Screens.SelectLanguageScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, discard, ($), (<$>))
import Screens.SelectLanguageScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import ModifyScreenState (modifyScreenState)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.SelectLanguageScreen.View as SelectLanguageScreen
import Types.App (FlowBT, GlobalState(..), SELECT_LANGUAGE_SCREEN_OUTPUT(..),ScreenType(..))


selectLanguageScreen :: FlowBT String SELECT_LANGUAGE_SCREEN_OUTPUT
selectLanguageScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ SelectLanguageScreen.screen state.selectLanguageScreen
  case action of
    GoToHomeScreen -> App.BackT $ App.NoBack <$> (pure $ GO_TO_HOME_SCREEN)
    UpdateLanguage state -> do   
                            modifyScreenState $ SelectLanguageScreenStateType (\selectLanguageScreen -> state)
                            App.BackT $ App.NoBack <$> (pure $ UPDATE_LANGUAGE state)
