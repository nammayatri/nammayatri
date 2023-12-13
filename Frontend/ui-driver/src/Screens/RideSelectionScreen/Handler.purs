{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.RideSelectionScreen.Handler where

import Control.Monad.Except.Trans (lift)
import Data.Maybe (Maybe(..))
import Engineering.Helpers.BackTrack (getState)
import Engineering.Helpers.Commons (liftFlow)
import Prelude (bind, pure, ($), (<$>))
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import PrestoDOM.Core (getPushFn)
import Screens.RideSelectionScreen.Controller (ScreenOutput(..))
import Screens.Types (AnimationState(..))
import Types.App (FlowBT, GlobalState(..), RIDES_SELECTION_SCREEN_OUTPUT(..))
import Control.Transformers.Back.Trans as App
import Components.IndividualRideCard.View (selectView) as IndividualRideCard
import PrestoDOM.List as PrestoList
import Screens.RideSelectionScreen.View as RideSelectionScreen

rideSelection :: FlowBT String RIDES_SELECTION_SCREEN_OUTPUT
rideSelection = do
  (GlobalState state) <- getState
  push <- lift $ lift $ liftFlow $ getPushFn Nothing "RideSelectionScreen"
  rideListItem <- lift $ lift $ PrestoList.preComputeListItem $ IndividualRideCard.selectView push
  act <- lift $ lift $ runScreen $ RideSelectionScreen.screen state.rideSelectionScreen { shimmerLoader = AnimatedIn } rideListItem
  case act of
    GoBack -> App.BackT $ pure App.GoBack
    SelectRide updatedState -> App.BackT $ App.BackPoint <$> (pure $ SELECT_RIDE updatedState)
    LoaderOutput updatedState -> App.BackT $ App.NoBack <$> (pure $ LOADER_RIDES_OUTPUT updatedState)
    RefreshScreen updatedState -> App.BackT $ App.NoBack <$> (pure $ REFRESH_RIDES updatedState)
