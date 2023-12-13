{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.PermissionsScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard)
import Presto.Core.Types.Language.Flow (getLogFields)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.PermissionsScreen.Controller (ScreenOutput(..))
import Screens.PermissionsScreen.View as PermissionsScreen
import Types.App (FlowBT, GlobalState(..), PERMISSIONS_SCREEN_OUTPUT(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)

permissions :: FlowBT String PERMISSIONS_SCREEN_OUTPUT
permissions = do
  (GlobalState state) <- getState
  logField_ <- lift $ lift $ getLogFields
  action <- lift $ lift $ runScreen $ PermissionsScreen.screen state.permissionsScreen { data { logField = logField_ } }
  case action of
    GoBack -> App.BackT $ pure App.GoBack
    GoToHome -> App.BackT $ App.BackPoint <$> pure DRIVER_HOME_SCREEN
    LogoutAccount -> App.BackT $ App.BackPoint <$> pure LOGOUT_FROM_PERMISSIONS_SCREEN
    GoToRegisteration updatedState -> do
      modifyScreenState $ PermissionsScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_REGISTERATION_SCREEN updatedState)
