{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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
        LogoutAccount -> App.BackT $ App.BackPoint <$> pure LOG_OUT
