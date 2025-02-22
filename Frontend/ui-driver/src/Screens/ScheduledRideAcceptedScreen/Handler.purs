module Screens.ScheduledRideAcceptedScreen.Handler where

import Prelude
import Types.App 
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Control.Monad.Except.Trans (lift)
import PrestoDOM.Core.Types.Language.Flow (runLoggableScreen)
import Types.ModifyScreenState (modifyScreenState)
import Control.Transformers.Back.Trans (BackT(..), FailBack(..)) as App
import Components.RideRequestCard.View as RideRequestCard
import Engineering.Helpers.Commons (liftFlow)
import PrestoDOM.Core (getPushFn)
import PrestoDOM
import Data.Maybe (Maybe(..))
import PrestoDOM.List as PrestoList
import Presto.Core.Types.Language.Flow (getLogFields, setLogField)
import Screens.ScheduledRideAcceptedScreen.View as ScheduledRideAcceptedScreen
import Screens.ScheduledRideAcceptedScreen.ScreenData
import Screens.ScheduledRideAcceptedScreen.Controller

scheduledRideAcceptedScreen :: FlowBT String SCHEDULED_RIDE_ACCEPTED_SCREEN_OUTPUT
scheduledRideAcceptedScreen = do
  (GlobalState state) <- getState
  act <- lift $ lift $ runLoggableScreen $ ScheduledRideAcceptedScreen.screen state.scheduledRideAcceptedScreen
  case act of
    GoHome -> App.BackT $ App.BackPoint <$> pure (GO_HOME_FROM_SCHEDULED_RIDE_ACCEPT_SCREEN )
