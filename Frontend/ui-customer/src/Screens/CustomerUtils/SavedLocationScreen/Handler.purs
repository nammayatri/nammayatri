{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.SavedLocationScreen.Handler where

import Prelude (bind, discard, ($), (<$>), pure)
import Engineering.Helpers.BackTrack (getState)
import Screens.SavedLocationScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.SavedLocationScreen.View as SavedLocationScreen
import Types.App (FlowBT, GlobalState(..), SAVED_LOCATION_SCREEN_OUTPUT(..))
import ModifyScreenState (modifyScreenState)
import Storage (setValueToLocalStore, KeyStore(..))
import Types.App (ScreenType(..))

savedLocationScreen :: FlowBT String SAVED_LOCATION_SCREEN_OUTPUT
savedLocationScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ SavedLocationScreen.screen state.savedLocationScreen (GlobalState state)
  case act of
    AddLocation updatedState -> do
      modifyScreenState $ SavedLocationScreenStateType (\savedLocationScreenState → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ ADD_NEW_LOCATION updatedState)
    DeleteLocation tagName -> App.BackT $ App.NoBack <$> (pure $ DELETE_LOCATION tagName)
    EditLocation cardState -> App.BackT $ App.BackPoint <$> (pure $ EDIT_LOCATION cardState)
    GoBack -> do
      _ <- pure $ setValueToLocalStore TRACKING_DRIVER "False"
      App.BackT $ App.NoBack <$> (pure $ GO_BACK_FROM_SAVED_LOCATION)
