module Screens.MyRidesScreen.Handler where

import Components.IndividualRideCard.View as IndividualRideCard
import Components.SettingSideBar.Controller as SettingSideBarController
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans as App
import Data.Maybe (Maybe(..))
import Engineering.Helpers.BackTrack (getState)
import Engineering.Helpers.Commons (liftFlow)
import ModifyScreenState (modifyScreenState)
import Prelude (bind, ($), (<$>), discard, pure)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import PrestoDOM.Core2 (getPushFn)
import PrestoDOM.List as PrestoList
import Screens.MyRidesScreen.Controller (ScreenOutput(..))
import Screens.MyRidesScreen.ScreenData (dummyIndividualCard)
import Screens.MyRidesScreen.View as MyRidesScreen
import Screens.Types (IndividualRideCardState, AnimationState(..))
import Types.App (FlowBT, GlobalState(..), MY_RIDES_SCREEN_OUTPUT(..), ScreenType(..))


myRidesScreen :: FlowBT String MY_RIDES_SCREEN_OUTPUT
myRidesScreen = do
  (GlobalState state) <- getState
  push <- lift $ lift $ liftFlow $ getPushFn Nothing "MyRidesScreen"
  listItemm <- lift $ lift $ PrestoList.preComputeListItem $ IndividualRideCard.view push listItem1
  act <- lift $ lift $ runScreen $ MyRidesScreen.screen state.myRidesScreen{shimmerLoader = AnimatedIn} listItemm
  case act of 
    GoBack updatedState -> do
      if  (updatedState.props.fromNavBar) then do 
        modifyScreenState $ HomeScreenStateType (\homeScreenState -> homeScreenState{data{settingSideBar{opened = SettingSideBarController.OPEN}}})  
        App.BackT $ App.BackPoint <$> (pure $ GO_TO_NAV_BAR)
        else App.BackT $ App.BackPoint <$> (pure $ GO_TO_HELP_SCREEN)
    MyRidesScreen updatedState -> App.BackT $ App.BackPoint <$> (pure $ REFRESH updatedState)
    GoToTripDetails updatedState -> do
      modifyScreenState $ TripDetailsScreenStateType (\tripDetailsScreen → tripDetailsScreen{ data {tripId = (updatedState.data.selectedItem).shortRideId ,source = updatedState.data.selectedItem.source, destination = updatedState.data.selectedItem.destination,date =updatedState.data.selectedItem.date, time = updatedState.data.selectedItem.time, rating = updatedState.data.selectedItem.rating, driverName = updatedState.data.selectedItem.driverName,totalAmount = updatedState.data.selectedItem.totalAmount , selectedItem = updatedState.data.selectedItem}})
      App.BackT $ App.BackPoint <$> (pure $ TRIP_DETAILS updatedState)
    LoaderOutput updatedState -> App.BackT $ App.BackPoint <$> (pure $ LOADER_OUTPUT updatedState)
    BookRide -> App.BackT $ App.BackPoint <$> (pure $ BOOK_RIDE )
    RepeatRide updatedState-> App.BackT $ App.NoBack <$> (pure $ REPEAT_RIDE_FLOW updatedState)

listItem1 :: IndividualRideCardState
listItem1 = dummyIndividualCard
