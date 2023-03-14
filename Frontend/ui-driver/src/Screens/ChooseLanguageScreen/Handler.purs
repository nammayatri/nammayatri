module Screens.ChooseLanguageScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard)
import Screens.ChooseLanguageScreen.Controller (ScreenOutput(..))
import Types.App (ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)


import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.ChooseLanguageScreen.View as ChooseLanguageScreen
import Types.App (FlowBT, GlobalState(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)


chooseLanguage :: FlowBT String ScreenOutput
chooseLanguage = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ ChooseLanguageScreen.screen state.chooseLanguageScreen
  case action of
    GoToEnterMobileScreen updateState -> do
      modifyScreenState $ ChooseLanguageScreenStateType (\chooseLanguageScreenScreen -> updateState)
      App.BackT $ App.BackPoint <$> pure action
