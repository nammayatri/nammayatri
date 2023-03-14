module Screens.ChooseLanguageScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, discard, ($), (<$>))
import Screens.ChooseLanguageScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import ModifyScreenState (modifyScreenState)
import Screens.ChooseLanguageScreen.View as ChooseLanguageScreen
import Types.App (FlowBT, GlobalState(..), ScreenType(..))


chooseLanguageScreen :: FlowBT String ScreenOutput
chooseLanguageScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ ChooseLanguageScreen.screen state.chooseLanguageScreen
  case action of
    NextScreen language -> App.BackT $ App.NoBack <$> (pure action)
    Refresh state -> do 
                  modifyScreenState $ ChooseLanguageScreenStateType (\chooseLanguageScreen -> state)
                  App.BackT $ App.NoBack <$> (pure action)
