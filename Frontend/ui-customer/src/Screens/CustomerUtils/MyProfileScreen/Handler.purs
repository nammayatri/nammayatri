module Screens.MyProfileScreen.Handler where

import Prelude (Unit, bind, pure, ($), (<$>))
import Engineering.Helpers.BackTrack (getState)
import Screens.MyProfileScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.MyProfileScreen.View as MyProfileScreen
import Types.App (FlowBT, GlobalState(..), MY_PROFILE_SCREEN_OUTPUT(..))


myProfileScreen :: FlowBT String MY_PROFILE_SCREEN_OUTPUT
myProfileScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ MyProfileScreen.screen state.myProfileScreen
  case action of
    GoToHomeScreen -> App.BackT $ pure App.GoBack
    UpdateProfile state -> App.BackT $ App.NoBack <$> (pure $ UPDATE_USER_PROFILE state)
    DeleteAccount updatedState -> App.BackT $ App.NoBack <$> pure ( DELETE_ACCOUNT updatedState)
    GoToHome -> App.BackT $ App.NoBack <$> pure (GO_TO_HOME_)