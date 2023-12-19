{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}


module Screens.ChooseCityScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, ($), pure, (<$>), discard)
import Screens.ChooseCityScreen.Controller (ScreenOutput(..))
import Screens.ChooseCityScreen.View as ChooseCityScreen
import Types.App (CHOOSE_CITY_SCREEN_OUTPUT(..), FlowBT, GlobalState(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)
import React.Navigation.Navigate (navigateToScreen)


chooseCityScreen :: FlowBT String CHOOSE_CITY_SCREEN_OUTPUT
chooseCityScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ navigateToScreen $ ChooseCityScreen.screen state.chooseCityScreen
  case act of
    SelectLanguageScreen -> App.BackT $ pure App.GoBack 
    WelcomeScreen -> App.BackT $ App.NoBack <$> (pure GoToWelcomeScreen)
    RefreshScreen updatedState -> do
      modifyScreenState $ ChooseCityScreenStateType (\chooseCityScreen -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ REFRESH_SCREEN_CHOOSE_CITY updatedState)
