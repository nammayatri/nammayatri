module Screens.PermissionsScreen.Handler where

import Prelude (bind, pure, ($), (<$>))
import Engineering.Helpers.BackTrack (getState)
import Screens.PermissionsScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.PermissionsScreen.View as PermissionsScreen
import Types.App (FlowBT, GlobalState(..), PERMISSIONS_SCREEN_OUTPUT(..))

permissions :: FlowBT String PERMISSIONS_SCREEN_OUTPUT
permissions = do
    (GlobalState state) <- getState
    action <- lift $ lift $ runScreen $ PermissionsScreen.screen state.permissionsScreen
    case action of 
        GoBack -> App.BackT $ pure App.GoBack
        GoToHome -> App.BackT $ App.BackPoint <$> pure DRIVER_HOME_SCREEN
