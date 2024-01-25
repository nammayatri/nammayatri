-- {-
 
--   Copyright 2022-23, Juspay India Pvt Ltd
 
--   This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
--   as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
--   is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
--   or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
--   the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-- -}
module Screens.NammaSafetyScreen.VideoRecordScreen.Handler where

import Prelude

-- import Control.Monad.Except.Trans (lift)
-- import Control.Transformers.Back.Trans as App
-- import Engineering.Helpers.BackTrack (getState)
-- import ModifyScreenState (modifyScreenState)
-- import PrestoDOM.Core.Types.Language.Flow (runScreen)
-- import Screens.NammaSafetyScreen.VideoRecordScreen.Controller (ScreenOutput(..))
-- import Screens.NammaSafetyScreen.VideoRecordScreen.View as NammaSafetyScreen
-- import Types.App (FlowBT, GlobalState(..), NAMMA_SAFETY_SCREEN_OUTPUT(..), ScreenType(..))
-- import Debug

-- videoScreen :: FlowBT String NAMMA_SAFETY_SCREEN_OUTPUT
-- videoScreen = do
--   (GlobalState state') <- getState
--   _ <- pure $ spy "videoScreen" "videoScreen"
--   act <- lift $ lift $ runScreen $ NammaSafetyScreen.screen state'.nammaSafetyScreen
--   case act of
--     GoBack updatedState -> do
--       modifyScreenState $ NammaSafetyScreenStateType (\_ -> updatedState)
--       App.BackT $ App.NoBack <$> (pure $ GO_BACK_FROM_SAFETY_SCREEN updatedState)
