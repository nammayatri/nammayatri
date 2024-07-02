module Screens.EarningsScreen.Controller where

import Data.Maybe
import Effect
import Prelude
import PrestoDOM
import Screens.EarningsScreen.ScreenData

import Services.API (RidesSummary(..))

data ScreenOutput
  = NextScreen State
  | Back State
  | GoToWeekly State

data Action
  = NextClick
  | BackClick
  | ChangeTab
  | ToggleInfoView
  | RemovePopup
  | UpdateRideData (Array RidesSummary)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

eval :: Action -> State -> Eval Action ScreenOutput State
eval NextClick state = exit $ NextScreen state

eval ToggleInfoView state = continue state { props { showInfoView = not state.props.showInfoView } }

eval RemovePopup state = continue state { props { showInfoView = not state.props.showInfoView } }

eval ChangeTab state = exit $ GoToWeekly state

eval _ state = continue state
