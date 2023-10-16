{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideHistoryScreen.Handler where

import Components.IndividualRideCard as IndividualRideCard
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Data.Maybe (Maybe(..))
import Engineering.Helpers.BackTrack (getState)
import Engineering.Helpers.Commons (liftFlow)
import Prelude (bind, map, pure, ($), (<$>), discard)
import PrestoDOM.Core (getPushFn)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import PrestoDOM.List as PrestoList
import Screens.RideHistoryScreen.Controller (ScreenOutput(..))
import Screens.RideHistoryScreen.ScreenData (initData) as RideHistoryScreenData
import Screens.RideHistoryScreen.View as RideHistoryScreen
import Screens.Types (IndividualRideCardState, AnimationState(..))
import Services.API (RidesInfo(..), Status(..))
import Types.App (FlowBT, GlobalState(..), MY_RIDES_SCREEN_OUTPUT(..), NAVIGATION_ACTIONS(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)


rideHistory :: FlowBT String MY_RIDES_SCREEN_OUTPUT
rideHistory = do
  (GlobalState state) <- getState
  push <- lift $ lift $ liftFlow $ getPushFn Nothing "RideHistoryScreen"
  rideListItem <- lift $ lift $ PrestoList.preComputeListItem $ IndividualRideCard.view push
  act <- lift $ lift $ runScreen $ RideHistoryScreen.screen state.rideHistoryScreen{shimmerLoader = AnimatedIn} rideListItem
  case act of
    GoBack -> App.BackT $ pure App.GoBack
    HomeScreen -> do
      modifyScreenState $ RideHistoryScreenStateType (\_ -> RideHistoryScreenData.initData)
      App.BackT $ App.BackPoint <$> (pure $ HOME_SCREEN )
    ProfileScreen -> do
      modifyScreenState $ RideHistoryScreenStateType (\_ -> RideHistoryScreenData.initData)
      App.BackT $ App.BackPoint <$> (pure $ PROFILE_SCREEN )
    GoToReferralScreen -> do
      modifyScreenState $ RideHistoryScreenStateType (\_ -> RideHistoryScreenData.initData)
      App.BackT $ App.BackPoint <$> pure GO_TO_REFERRAL_SCREEN
    GoToTripDetails updatedState -> do
      modifyScreenState $ RideHistoryScreenStateType (\_ -> updatedState{currentTab = updatedState.currentTab})
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_TRIP_DETAILS updatedState.selectedItem)
    LoaderOutput updatedState -> App.BackT $ App.NoBack <$> (pure $ LOADER_OUTPUT updatedState)
    RefreshScreen updatedState -> App.BackT $ App.NoBack <$> (pure $ REFRESH updatedState)
    GoToFilter currentTab -> App.BackT $ App.BackPoint <$> (pure $ FILTER currentTab)
    GoToNotification -> do 
      modifyScreenState $ RideHistoryScreenStateType (\_ -> RideHistoryScreenData.initData)
      App.BackT $ App.BackPoint <$> (pure $ NOTIFICATION_FLOW)
    SelectedTab updatedState -> do
      modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> rideHistoryScreen{currentTab = updatedState.currentTab, offsetValue = 0})
      App.BackT $ App.NoBack <$> (pure $ SELECTED_TAB updatedState)
    OpenPaymentHistoryScreen updatedState -> do
      modifyScreenState $ RideHistoryScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ OPEN_PAYMENT_HISTORY updatedState)
    SubscriptionScreen updatedState -> do
      modifyScreenState $ RideHistoryScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ (RIDE_HISTORY_NAV GoToSubscription))
