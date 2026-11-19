module Screens.RideBookingFlow.RiderRideCompletedCard.Handler where

import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, ($), (<$>), discard)
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.RideBookingFlow.RiderRideCompletedCard.Controller (ScreenOutput(..))
import Screens.RideBookingFlow.RiderRideCompletedCard.View as RiderRideCompletedScreen
import Types.App (FlowBT, GlobalState(..), RIDER_RIDECOMPLETED_SCREEN_OP(..), ScreenType(..), RIDER_RIDECOMPLETED_SCREEN_OP(..))
import ModifyScreenState (modifyScreenState)
import Screens.HomeScreen.Transformer(getTripDetailsState)

riderRideCompletedScreen :: FlowBT String RIDER_RIDECOMPLETED_SCREEN_OP
riderRideCompletedScreen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ RiderRideCompletedScreen.screen state.riderRideCompletedScreen
  case action of
    RideDetailsScreen updatedState -> do 
      modifyScreenState $ RiderRideCompletedScreenStateType (\_ -> updatedState)
      modifyScreenState $ TripDetailsScreenStateType (\_ -> getTripDetailsState updatedState.ratingViewState.rideBookingRes state.tripDetailsScreen)
      App.BackT $ App.BackPoint <$> (pure $ RIDER_DETAILS_SCREEN updatedState)
    GoToHelpAndSupport updatedState -> do
      modifyScreenState $ RiderRideCompletedScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_HELP_AND_SUPPORTS)
    HomeScreen updatedState -> do 
      modifyScreenState $ RiderRideCompletedScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ HOME_SCREENS updatedState.rideId)
    GoToNammaSafety updatedState triggerSos showTestDrill -> do
      modifyScreenState $ RiderRideCompletedScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GOTO_NAMMASAFETY updatedState triggerSos showTestDrill)
    SubmitRating updatedState audio -> do
      modifyScreenState $ RiderRideCompletedScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ SUBMIT_RATINGS updatedState audio)
    GoToDriversProfile updatedState -> do
      modifyScreenState $ RiderRideCompletedScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_DRIVER_PROFILE updatedState)
    GoToIssueReportChatScreenWithIssue updatedState issueType -> do
      modifyScreenState $ RiderRideCompletedScreenStateType (\_ -> updatedState)
      App.BackT $ App.BackPoint <$> (pure $ GO_TO_ISSUE_REPORT_CHAT_SCREEN_WITH_ISSUE updatedState issueType)
    _ -> App.BackT $ pure App.GoBack