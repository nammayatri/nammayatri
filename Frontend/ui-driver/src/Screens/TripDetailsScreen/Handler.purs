{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.TripDetailsScreen.Handler where

import Prelude (bind, ($), pure, (<$>))
import Engineering.Helpers.BackTrack (getState)
import Screens.TripDetailsScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.TripDetailsScreen.View as TripDetailsScreen
import Types.App (FlowBT, GlobalState(..), TRIP_DETAILS_SCREEN_OUTPUT(..))

tripDetailsScreen :: FlowBT String TRIP_DETAILS_SCREEN_OUTPUT
tripDetailsScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ TripDetailsScreen.screen state.tripDetailsScreen
  case act of
    GoBack -> App.BackT $ pure App.GoBack
    OnSubmit -> App.BackT $ App.BackPoint <$> (pure $ ON_SUBMIT)
    GoHome -> App.BackT $ App.BackPoint <$> (pure $ GO_TO_HOME_SCREEN)
    GoToHelpAndSupport -> App.BackT $ App.BackPoint <$> (pure $ OPEN_HELP_AND_SUPPORT)
