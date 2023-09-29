{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideSelectionScreen.Handler where

import Components.IndividualRideCard.View as IndividualRideCard
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Data.Maybe (Maybe(..))
import Engineering.Helpers.BackTrack (getState)
import Engineering.Helpers.Commons (liftFlow)
import Prelude (bind, ($), (<$>), discard, pure, (<<<))
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import PrestoDOM.Core (getPushFn)
import PrestoDOM.List as PrestoList
import Screens.RideSelectionScreen.Controller (ScreenOutput(..), Action(..))
import Screens.RideSelectionScreen.ScreenData (dummyIndividualCard)
import Screens.RideSelectionScreen.View as RideSelectionScreen
import Screens.Types (IndividualRideCardState, AnimationState(..))
import Types.App (FlowBT, GlobalState(..), RIDE_SELECTION_SCREEN_OUTPUT(..), ScreenType(..))
import Presto.Core.Types.Language.Flow (getLogFields)

rideSelection :: FlowBT String RIDE_SELECTION_SCREEN_OUTPUT
rideSelection = do
  (GlobalState state) <- getState
  push <- lift $ lift $ liftFlow $ getPushFn Nothing "RideSelectionScreen"
  listItemm <- lift $ lift $ PrestoList.preComputeListItem $ IndividualRideCard.view (push <<< IndividualRideCardActionController) listItem1
  logField_ <- lift $ lift $ getLogFields
  act <- lift $ lift $ runScreen $ RideSelectionScreen.screen state.rideSelectionScreen{shimmerLoader = AnimatedIn , data{logField = logField_}} listItemm
  case act of 
    GoBack -> App.BackT $ pure App.GoBack
    LoaderOutput  updatedState -> App.BackT $ App.NoBack <$> (pure $ LOADER_RIDES_OUTPUT updatedState)
    SelectRide    updatedState -> App.BackT $ App.BackPoint <$> (pure $ SELECT_RIDE updatedState)
    RefreshScreen updatedState -> App.BackT $ App.NoBack <$> (pure $ REFRESH_RIDES updatedState)

listItem1 :: IndividualRideCardState
listItem1 = dummyIndividualCard
