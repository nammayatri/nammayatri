{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.TripDetailsScreen.Handler where

import Prelude (bind, discard, ($), pure, (<$>))
import Engineering.Helpers.BackTrack (getState)
import Screens.TripDetailsScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.TripDetailsScreen.View as TripDetailsScreen
import Types.App (FlowBT, GlobalState(..), TRIP_DETAILS_SCREEN_OUTPUT(..), ScreenType(..))
import ModifyScreenState (modifyScreenState)
import Screens.Types (TripDetailsGoBackType(..))

tripDetailsScreen :: FlowBT String TRIP_DETAILS_SCREEN_OUTPUT
tripDetailsScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ TripDetailsScreen.screen state.tripDetailsScreen
  case act of
    GoBack fromMyRides -> do
      modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> tripDetailsScreen { props { issueReported = false } })
      case fromMyRides of
        Home -> App.BackT $ pure App.GoBack
        MyRides -> App.BackT $ App.NoBack <$> (pure $ GO_TO_RIDES)
        HelpAndSupport -> App.BackT $ App.NoBack <$> (pure $ GO_TO_HELPSCREEN)
    OnSubmit state -> App.BackT $ App.NoBack <$> (pure $ ON_SUBMIT state)
    GoToInvoice updatedState -> App.BackT $ App.BackPoint <$> (pure $ GO_TO_INVOICE updatedState)
    GoHome updatedState -> do
      modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen -> updatedState { props { issueReported = false } })
      App.BackT $ App.NoBack <$> (pure $ GO_TO_HOME updatedState)
    ConnectWithDriver updatedState -> App.BackT $ App.NoBack <$> (pure $ CONNECT_WITH_DRIVER updatedState)
