module Screens.DocumentDetailsScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>))
import PrestoDOM.Core.Types.Language.Flow (runLoggableScreen)
import Screens.DocumentDetailsScreen.Controller (ScreenOutput(..))
import Screens.DocumentDetailsScreen.View as DocumentDetailsScreen
import Types.App (DOCUMENT_DETAILS_SCREEN_OUTPUT(..), FlowBT, GlobalState(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)

documentDetailsScreen :: FlowBT String DOCUMENT_DETAILS_SCREEN_OUTPUT
documentDetailsScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runLoggableScreen $ DocumentDetailsScreen.screen state.documentDetailsScreen
  case action of
    _ -> App.BackT $ pure App.GoBack
