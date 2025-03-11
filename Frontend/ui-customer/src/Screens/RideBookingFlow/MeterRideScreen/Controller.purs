module Screens.RideBookingFlow.MeterRideScreen.Controller where

import Prelude
import Screens.Types as ST
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit)
import Data.Maybe (Maybe(..))
import JBridge as JB
import PrestoDOM.Types.Core (class Loggable)

data Action = NoAction
            | BackPressed

instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = pure unit

data ScreenOutput = GoBack ST.MeterRideScreenState

eval :: Action -> ST.MeterRideScreenState -> Eval Action ScreenOutput ST.MeterRideScreenState

eval BackPressed state = exit $ GoBack state

eval _ state = continue state