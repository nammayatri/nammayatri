{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.TicketBookingFlow.BusTrackingScreen.Handler where

import Prelude ((<$>), pure)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Control.Transformers.Back.Trans as App
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Prelude (Unit, bind, discard, void, ($))
import PrestoDOM.Core.Types.Language.Flow (initUIWithNameSpace, showScreenWithNameSpace, runScreen)
import Screens.TicketBookingFlow.BusTrackingScreen.View as BusTrackingScreen
import Screens.TicketBookingFlow.BusTrackingScreen.Controller (ScreenOutput(..))
import Types.App (GlobalState(..), FlowBT, ScreenType(..))
import ModifyScreenState (modifyScreenState)
import Data.Function.Uncurried
import LocalStorage.Cache

busTrackingScreen :: FlowBT String ScreenOutput
busTrackingScreen = do
  (GlobalState state') <- getState
  act <- lift $ lift $ runScreen $ BusTrackingScreen.screen state'.busTrackingScreen
  case act of
  
    _ -> do
      let _ = runFn2 setInCache "POLLING_ID" ""

      -- modifyScreenState $ JourneyTrackingScreenStateType (\_ -> state)
      App.BackT $ App.BackPoint <$> (pure $ act)