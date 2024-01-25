{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.NammaSafetyFlow.ActivateSafetyScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import ModifyScreenState (modifyScreenState)
import Prelude (bind, discard, ($), pure, (<$>))
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.NammaSafetyFlow.ActivateSafetyScreen.Controller (ScreenOutput(..))
import Screens.NammaSafetyFlow.ActivateSafetyScreen.View as ActivateSafetyScreen
import Types.App (FlowBT, GlobalState(..), NAMMA_SAFETY_SCREEN_OUTPUT(..), ScreenType(..))

activateSafetyScreen :: FlowBT String ScreenOutput
activateSafetyScreen = do
  (GlobalState state') <- getState
  act <- lift $ lift $ runScreen $ ActivateSafetyScreen.screen state'.nammaSafetyScreen
  case act of
    GoBack updatedState -> do
      modifyScreenState $ NammaSafetyScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ act)
    GoToEmergencyContactScreen updatedState -> do
      modifyScreenState $ NammaSafetyScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ act)
    GoToSosScreen updatedState -> do
      modifyScreenState $ NammaSafetyScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ act)
    GoToEducationScreen updatedState -> do
      modifyScreenState $ NammaSafetyScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ act)
    CreateSos updatedState isPoliceFlow -> do
      modifyScreenState $ NammaSafetyScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ act)
    GoToIssueScreen updatedState -> do
      modifyScreenState $ NammaSafetyScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ act)