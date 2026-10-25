module Screens.RideSummaryScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runLoggableScreen)
import Screens.RideSummaryScreen.ScreenData
import Screens.RideSummaryScreen.View as RideSummaryScreen
import Types.App (RIDE_SUMMARY_SCREEN_OUTPUT(..), GlobalState(..), FlowBT)
import Screens.RideSummaryScreen.Controller
import Types.ModifyScreenState (modifyScreenState)
import Types.App
import Debug

rideSummaryScreen :: FlowBT String RIDE_SUMMARY_SCREEN_OUTPUT
rideSummaryScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runLoggableScreen $ RideSummaryScreen.screen state.rideSummaryScreen
  case act of
    (AcceptScheduleRide bookingId)-> App.BackT $ App.BackPoint <$> pure (ACCEPT_SCHEDULED_RIDE bookingId)
    GoBack  -> App.BackT $ App.BackPoint <$> pure (GO_TO_RIDE_REQUEST state.rideSummaryScreen)
    (UpdateRouteInterCity slat slon dlat dlon)  -> App.BackT $ App.BackPoint <$> pure (UPDATE_ROUTE_INTERCITY slat slon dlat dlon state.rideSummaryScreen)
    (UpdateRouteRental lat lon)  -> App.BackT $ App.BackPoint <$> pure (UPDATE_ROUTE_RENTAL lat lon)
    (UpdateRouteRegular slat slon dlat dlon)  ->  App.BackT $ App.BackPoint <$> pure (UPDATE_ROUTE_REGULAR slat slon dlat dlon state.rideSummaryScreen)
    (CancelRide id info reason) ->  App.BackT $ App.BackPoint <$> (pure $ CANCEL_SCHEDULED_RIDE {id  : id , info : info ,reason : reason} )
    (DoneButtonClicked ) ->  App.BackT $ App.BackPoint <$> pure (BACK_HOME) 
    CallingCustomer updatedState exophoneNumber -> do
      modifyScreenState $ RideSummaryScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> (pure $ ON_CALLING updatedState exophoneNumber)
    GoToOpenGoogleMap updatedState -> do
      modifyScreenState $ RideSummaryScreenStateType (\_ -> updatedState)
      App.BackT $ App.NoBack <$> (pure $ GO_TO_OPEN_GOOGLE_MAP updatedState)
    PressedBack -> App.BackT $ App.BackPoint <$> pure (GO_TO_HOME_SCREEN_FROM_BANNER)
    (FcmNotification notificationType screenState) -> do 
      modifyScreenState $ RideSummaryScreenStateType (\_ → screenState)
      App.BackT $ App.BackPoint <$> (pure $ FCM_NOTIFICATION_TYPE notificationType screenState)
    GotoRideRequestscreen updatedState ->do
      modifyScreenState $ RideSummaryScreenStateType (\_ → updatedState)
      App.BackT $ App.BackPoint <$> pure (GO_BACK_TO_RIDE_REQUEST)



  