module Screens.RideHistoryScreen.Handler where

import Prelude (bind, map, pure, ($), (<$>), discard)
import Engineering.Helpers.BackTrack (getState)
import Screens.RideHistoryScreen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Engineering.Helpers.Commons (liftFlow)
import Screens.RideHistoryScreen.View as RideHistoryScreen
import Types.App (FlowBT, GlobalState(..), MY_RIDES_SCREEN_OUTPUT(..),ScreenType(..))
import Screens.Types (IndividualRideCardState, AnimationState(..))
import Components.IndividualRideCard as IndividualRideCard
import PrestoDOM.List as PrestoList
import PrestoDOM.Core2 (getPushFn)
import Services.APITypes (RidesInfo(..), Status(..))
import Data.Maybe (Maybe(..))
import Types.ModifyScreenState (modifyScreenState)



rideHistory :: FlowBT String MY_RIDES_SCREEN_OUTPUT
rideHistory = do
  (GlobalState state) <- getState
  push <- lift $ lift $ liftFlow $ getPushFn Nothing "RideHistoryScreen"
  rideListItem <- lift $ lift $ PrestoList.preComputeListItem $ IndividualRideCard.view push
  act <- lift $ lift $ runScreen $ RideHistoryScreen.screen state.rideHistoryScreen{shimmerLoader = AnimatedIn} rideListItem
  case act of 
    GoBack -> App.BackT $ pure App.GoBack
    RideHistoryScreen updatedState -> App.BackT $ App.BackPoint <$> (pure $ MY_RIDE updatedState)
    HomeScreen -> App.BackT $ App.BackPoint <$> (pure $ HOME_SCREEN )
    ProfileScreen -> App.BackT $ App.BackPoint <$> (pure $ PROFILE_SCREEN )
    GoToReferralScreen -> App.BackT $ App.BackPoint <$> pure GO_TO_REFERRAL_SCREEN
    GoToTripDetails updatedState -> do
      modifyScreenState $ RideHistoryScreenStateType (\rideHistoryScreen -> rideHistoryScreen{currentTab = updatedState.currentTab})
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_TRIP_DETAILS updatedState.selectedItem)
    LoaderOutput updatedState -> App.BackT $ App.BackPoint <$> (pure $ LOADER_OUTPUT updatedState)
    RefreshScreen updatedState -> App.BackT $ App.BackPoint <$> (pure $ REFRESH updatedState) 
    GoToFilter currentTab -> App.BackT $ App.BackPoint <$> (pure $ FILTER currentTab)
    GoToNotification -> App.BackT $ App.BackPoint <$> (pure $ NOTIFICATION_FLOW)
rideHistoryItem :: IndividualRideCardState
rideHistoryItem = {
    date : "31/05/2022",
    time : "7:35pm",
    total_amount : 0,
    card_visibility : "gone",
    shimmer_visibility : "visible",
    rideDistance : "",
    status : "COMPLETED",
    vehicleModel : "",
    shortRideId : "",
    vehicleNumber : "",
    driverName : "",
    driverSelectedFare : 0,
    vehicleColor : "",
    id : "",
    updatedAt : "",
    source : "",
    destination : ""
}

