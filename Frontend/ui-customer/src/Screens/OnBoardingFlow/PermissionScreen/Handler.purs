{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.PermissionScreen.Handler where
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, ($), pure, (<$>))
import PrestoDOM.Core.Types.Language.Flow (showScreen)
import Presto.Core.Types.Language.Flow (doAff , getLogFields)
import Screens.PermissionScreen.Controller (ScreenOutput(..))
import Screens.PermissionScreen.View as PermissionScreen
import Types.App (FlowBT, GlobalState(..), PERMISSION_SCREEN_OUTPUT(..))

permissionScreen :: FlowBT String PERMISSION_SCREEN_OUTPUT
permissionScreen = do
  (GlobalState state) <- getState
  logField_ <- lift $ lift $ getLogFields
  act <- lift $ lift $ showScreen $ PermissionScreen.screen state.permissionScreen{logField = logField_}
  case act of
    GoBack -> App.BackT $ pure App.GoBack
    Refresh -> App.BackT $ App.BackPoint <$> (pure REFRESH_INTERNET)
    LocationCallBack updatedState -> App.BackT $ App.NoBack <$> (pure TURN_ON_GPS)
    InternetCallBack updatedState -> App.BackT $ App.NoBack <$> (pure TURN_ON_INTERNET)
