{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AboutUsScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>))
import Screens.AboutUsScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.AboutUsScreen.View as AboutUsScreen
import Types.App (ABOUT_US_SCREEN_OUTPUT(..), GlobalState(..), FlowBT)
import React.Navigation.Navigate (navigateToScreen)

aboutUsScreen :: FlowBT String ABOUT_US_SCREEN_OUTPUT
aboutUsScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ navigateToScreen $ AboutUsScreen.screen state.aboutUsScreen
  case action of
    GoBack -> App.BackT $ pure App.GoBack
    GoToHome updatedState -> App.BackT $ App.NoBack <$> (pure $ GO_TO_DRIVER_HOME_SCREEN)