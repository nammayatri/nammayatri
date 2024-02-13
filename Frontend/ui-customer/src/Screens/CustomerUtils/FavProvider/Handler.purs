{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.FavProviderScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, discard, ($), (<$>))
import Screens.FavProviderScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.FavProviderScreen.View as FavProviderScreen
import Types.App (FlowBT, GlobalState(..), FAV_PROVIDER_SCREEN_OUTPUT(..),ScreenType(..))
import ModifyScreenState (modifyScreenState)

favProviderScreen :: FlowBT String FAV_PROVIDER_SCREEN_OUTPUT
favProviderScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ FavProviderScreen.screen state.favProviderScreen
  case action of
    _ -> App.BackT $ App.NoBack <$> (pure $ GO_TO_SAVED_LOCATION)
