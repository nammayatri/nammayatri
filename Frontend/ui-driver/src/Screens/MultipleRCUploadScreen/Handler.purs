{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.MultipleRCUploadScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (Unit, bind, discard, pure, ($), (<$>))
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Types.ModifyScreenState (modifyScreenState)
import Screens.MultipleRCUploadScreen.Controller (ScreenOutput(..))
import Screens.MultipleRCUploadScreen.View as MultipleRCUploadScreenView
import Types.App (FlowBT, GlobalState(..), MULTIPL_RC_UPLOAD_SCREEN_OUTPUT(..), ScreenType(..))


multipleRCScreen :: String -> FlowBT String Unit
multipleRCScreen screenType = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ MultipleRCUploadScreenView.screen state.multipleRCUploadScreen screenType
  case action of
    GoToDriverDetailsScreen updatedState -> do 
      modifyScreenState $ MultipleRCUploadScreenType (\_ -> updatedState)
      -- App.BackT $ App.NoBack <$> pure $ GO_TO_VEHICLE_DETAILS updatedState
    GoToSuccessScreen updatedState -> do 
      modifyScreenState $ MultipleRCUploadScreenType (\_ -> updatedState)
      -- App.BackT $ App.NoBack <$> pure $ SUCCESS_SCREEN updatedState