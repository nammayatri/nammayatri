{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.AskPermissionScreen.Handler where

import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Presto.Core.Types.Language.Flow (doAff)
import Prelude (Unit, bind, discard, not, pure, unit, void, when, ($))
import Effect.Class (liftEffect)
import Control.Monad.Except.Trans (lift)
import PrestoDOM.Core.Types.Language.Flow (initUIWithNameSpace, showScreenWithNameSpace)
import Screens.AskPermissionScreen.View as AskPermissionScreen
import Types.App (FlowBT, GlobalState(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)
import Screens.Types as ST
import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import PrestoDOM.Core (terminateUI)
import Effect (Effect)

askPermissionScreen :: forall action. (Array ST.Permissions) -> Boolean -> (action -> Effect Unit) -> action -> Boolean -> FlowBT String Unit
askPermissionScreen permissionsArray online push action showAnim = do
  modifyScreenState $ AskPermissionScreenStateType $ \screenState -> screenState { data { permissionList = permissionsArray }, props { currentStep = permissionsArray !! 0, backpressEnabled = not online, ifAnim = showAnim } }
  (GlobalState state) <- getState
  when (not showAnim) $ void $ lift $ lift $ doAff $ liftEffect $ terminateUI $ Just "PermissionScreenV2"
  when showAnim $ liftFlowBT $ initUIWithNameSpace "PermissionScreenV2" Nothing
  void $ lift $ lift $ showScreenWithNameSpace $ AskPermissionScreen.screen state.askPermissionScreen
  liftFlowBT $ push action
  void $ lift $ lift $ doAff $ liftEffect $ terminateUI $ Just "PermissionScreenV2"
  pure unit
