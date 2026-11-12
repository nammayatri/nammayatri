module Screens.RideSummaryScreen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude
import Control.Monad.Except.Trans (lift)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.RideSummaryScreen.ScreenData
import Screens.RideSummaryScreen.View as RideSummaryScreen
import Types.App (RIDE_SUMMARY_SCREEN_OUTPUT(..), GlobalState(..), FlowBT,ScreenType(..))
import Screens.RideSummaryScreen.Controller
import ModifyScreenState (modifyScreenState)

rideSummaryScreen :: FlowBT String RIDE_SUMMARY_SCREEN_OUTPUT
rideSummaryScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runScreen $ RideSummaryScreen.screen state.rideSummaryScreen
  case act of
    (AcceptScheduleRide bookingId startTimeUTC)-> App.BackT $ App.BackPoint <$> pure (ACCEPT_SCHEDULED_RIDE bookingId startTimeUTC)
    GoBack -> App.BackT $ App.BackPoint <$> pure (GO_TO_RIDE_REQUEST)
    (GoToHomeScreen startTimeUTC bookingId fromScreen) ->  App.BackT $ App.BackPoint <$> pure (RIDE_CONFIRMED startTimeUTC fromScreen (bookingId))
    (CancelScheduledRide bookingId fromScreen)->  App.BackT $ App.BackPoint <$> pure (CANCEL_SCHEDULED_RIDE bookingId fromScreen)
    (NotificationListenerSO notificationType notificationBody) -> App.BackT $ App.BackPoint <$> pure (NOTIFICATION_LISTENER notificationType notificationBody)
    (RefreshScreenSO bookingId) -> App.BackT $ App.BackPoint <$> pure (REFRESH_RIDE_SUMMARY_SCREEN bookingId)
    CallDriverSO updatedState callType exophoneNumber -> do
        modifyScreenState $ RideSummaryScreenStateType (\_ -> updatedState)
        App.BackT $ App.BackPoint <$> (pure $ CALL_DRIVER updatedState callType exophoneNumber)