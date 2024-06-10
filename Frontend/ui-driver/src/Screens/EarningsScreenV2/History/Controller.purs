module Screens.EarningsScreen.History.Controller where

import Prelude
import Prelude
import Screens.EarningsScreen.ScreenData
import Prelude
import PrestoDOM
import PrestoDOM.List
import Effect
import Data.Maybe

data ScreenOutput
  = NextScreen State
  | Back State
  | GoToWeekly

data Action
  = NextClick
  | BackClick
  | SetRideListItem ListItem
  | SetPayoutListItem ListItem
  | BackPressed

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

eval :: Action -> State -> Eval Action ScreenOutput State
eval NextClick state = exit $ NextScreen state

eval (SetRideListItem item) state = continue state { data { rideListItem = Just item } }

eval (SetPayoutListItem item) state = continue state { data { payoutListItem = Just item } }

eval BackPressed state = exit GoToWeekly

eval _ state = update state
