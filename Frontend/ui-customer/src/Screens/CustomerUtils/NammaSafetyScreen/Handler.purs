{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.NammaSafetyScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Debug (spy)
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, discard, ($), pure, (<$>))
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.NammaSafetyScreen.Controller (ScreenOutput(..))
import Screens.NammaSafetyScreen.View as NammaSafetyScreen
import Types.App (GlobalState(..), FlowBT, ScreenType(..),defaultGlobalState)
import ModifyScreenState (modifyScreenState)


nammaSafetyScreen ::FlowBT String ScreenOutput
nammaSafetyScreen = do
  (GlobalState state') <- getState
  let (GlobalState defaultGlobalState') = defaultGlobalState
  act <- lift $ lift $ runScreen $ NammaSafetyScreen.screen state'.nammaSafetyScreen
  case act of
    GoBack -> do
                    _ <- pure $ spy "calll" ""
                    -- modifyScreenState $ NammaSafetyScreenStateType (\nammaSafetyOnBoard ->  state{ data { showOnboarding = true }})
                    App.BackT  $ App.BackPoint <$> pure act

-- REFERENCE TO UPDATE STATE GLOBALLY
-- modifyScreenState $ EnterMobileNumberScreenType (\enterMobileNumberScreen ->  state)