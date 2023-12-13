{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.SelectLanguageScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, discard, ($), (<$>))
import Screens.SelectLanguageScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import ModifyScreenState (modifyScreenState)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.SelectLanguageScreen.View as SelectLanguageScreen
import Types.App (FlowBT, GlobalState(..), SELECT_LANGUAGE_SCREEN_OUTPUT(..), ScreenType(..))

selectLanguageScreen :: FlowBT String SELECT_LANGUAGE_SCREEN_OUTPUT
selectLanguageScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ SelectLanguageScreen.screen state.selectLanguageScreen
  case action of
    GoToHomeScreen -> App.BackT $ App.NoBack <$> (pure $ GO_TO_HOME_SCREEN)
    UpdateLanguage state -> do
      modifyScreenState $ SelectLanguageScreenStateType (\selectLanguageScreen -> state)
      App.BackT $ App.NoBack <$> (pure $ UPDATE_LANGUAGE state)
