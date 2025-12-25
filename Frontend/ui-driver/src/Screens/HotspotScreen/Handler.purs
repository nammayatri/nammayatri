{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HotspotScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (pure, discard, ($), (<$>), bind, void)
import PrestoDOM.Core.Types.Language.Flow (runLoggableScreen)
import Presto.Core.Types.Language.Flow (getLogFields)
import Screens.HotspotScreen.Controller (ScreenOutput(..))
import Screens.HotspotScreen.View as HotspotScreen
import Types.App (FlowBT, GlobalState(..), HOTSPOT_SCREEN_OUTPUT(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)

hotspotScreen :: FlowBT String HOTSPOT_SCREEN_OUTPUT
hotspotScreen = do
    (GlobalState state) <- getState
    logField_ <- lift $ lift $ getLogFields
    action <- lift $ lift $ runLoggableScreen $ HotspotScreen.screen state.hotspotScreen{data{logField = logField_}}
    case action of
        RefreshingHotspots updatedState -> do
          modifyScreenState $ HotspotScreenStateType (\_ -> updatedState)
          App.BackT $ App.BackPoint <$> pure REFRESH_HOTSPOTS
        GoBack -> App.BackT $ App.NoBack <$> pure BACK_TO_HOMESCREEN