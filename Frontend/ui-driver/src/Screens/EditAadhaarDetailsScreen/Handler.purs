module Screens.EditAadhaarDetailsScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($))
import Screens.EditAadhaarDetailsScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.EditAadhaarDetailsScreen.View as EditAadhaarDetailsScreen
import Types.App (GlobalState(..), FlowBT, EDIT_AADHAAR_DETAILS_SCREEN_OUTPUT)

editAadhaarDetailsScreen :: FlowBT String EDIT_AADHAAR_DETAILS_SCREEN_OUTPUT
editAadhaarDetailsScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ EditAadhaarDetailsScreen.screen state.editAadhaarDetailsScreen
  case action of
    GoBack -> App.BackT $ pure App.GoBack