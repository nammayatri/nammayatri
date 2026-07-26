{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.MyProfileScreen.Handler where

import Prelude (bind, discard, pure, void, ($), (<$>))
import Engineering.Helpers.BackTrack (getState)
import Screens.MyProfileScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.MyProfileScreen.View as MyProfileScreen
import Types.App (FlowBT, GlobalState(..), MY_PROFILE_SCREEN_OUTPUT(..), ScreenType(..))
import ModifyScreenState (modifyScreenState)


myProfileScreen :: FlowBT String MY_PROFILE_SCREEN_OUTPUT
myProfileScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ MyProfileScreen.screen state.myProfileScreen
  case action of
    UpdateProfile updatedState -> do
      void $ modifyScreenState $ MyProfileScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ UPDATE_USER_PROFILE updatedState)
    GoToHome updatedState -> do
      void $ modifyScreenState $ MyProfileScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> pure GO_TO_HOME_