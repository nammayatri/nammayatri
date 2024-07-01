module Screens.EarningsScreen.Weekly.Controller where

import Prelude
import PrestoDOM
import Effect
import Data.Maybe
import Screens.EarningsScreen.ScreenData

data ScreenOutput
  = NextScreen State
  | Back State
  | GoToDaily State

data Action
  = NextClick
  | BackClick
  | ChangeTab
  | ToggleInfoView
  | RemovePopup
  | ShowTips
  | ShowAdjustments

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

eval :: Action -> State -> Eval Action ScreenOutput State
eval NextClick state = exit $ NextScreen state

eval ChangeTab state = exit $ GoToDaily state

eval ToggleInfoView state = continue state { props { rideDistanceInfoPopUp = not state.props.rideDistanceInfoPopUp } }

eval RemovePopup state = continue state { props { rideDistanceInfoPopUp = not state.props.rideDistanceInfoPopUp } }

eval ShowAdjustments state = continue state { data { prevAdjustmentRotation = state.data.adjustmentRotation, adjustmentRotation = if state.data.adjustmentRotation == 0.0 then 180.0 else 0.0 } }

eval _ state = update state
