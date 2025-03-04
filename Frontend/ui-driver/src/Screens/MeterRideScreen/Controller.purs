module Screens.MeterRideScreen.Controller where

import Screens.Types (MeterRideScreenState)
import Data.Maybe
import Prelude
import PrestoDOM.Core (getPushFn)
import PrestoDOM.Types.Core (class Loggable, Eval)
import PrestoDOM
import Timers (clearTimerWithId, startTimer)
import Components.TripStageTopBar.Controller as TripStageTopBar

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = pure unit

data Action = NoAction
            | BackPressed
            | ShowRateCard
            | CloseRateCard
            | HandleStartButton
            | MeterRideTimerCallback Int String String
            | ChangeSlider Boolean
            | SliderCallback Int
            | TripStageTopBarAC TripStageTopBar.Action
            | EnterDestination
            
data ScreenOutput = GoBack MeterRideScreenState 
                  | GoToEnterDestination MeterRideScreenState

eval :: Action -> MeterRideScreenState -> Eval Action ScreenOutput MeterRideScreenState

eval BackPressed state = do
  if state.props.startButtonCountDown <= 3 then do
    void $ pure $ clearTimerWithId "MeterRideStartTimer"
    exit $ GoBack state {props {startButtonCountDown = 5}}
  else exit $ GoBack state

eval ShowRateCard state = continue state {props {showRateCard = true}}

eval CloseRateCard state = continue state {props {showRateCard = false}}

eval HandleStartButton state = do

    if state.props.startButtonCountDown <= 3 then do
      void $ pure $ clearTimerWithId "MeterRideStartTimer"
      continue state { props { startButtonCountDown = 5 } }
    else
     continueWithCmd state { props { startButtonCountDown = 3 } } [ do
            push <- getPushFn Nothing "MeterRideScreen"
            void $ startTimer 4 "MeterRideStartTimer" "1" push MeterRideTimerCallback
            pure NoAction
    ]

eval (MeterRideTimerCallback seconds status timerId) state = do
  if status == "EXPIRED" then do
    void $ pure $ clearTimerWithId "MeterRideStartTimer"
    continue state {props {isMeterRideStarted = true}}
  else 
    continue state {props {startButtonCountDown = seconds}}

eval (SliderCallback val) state = continue state {props {sliderVal = val}}

eval (ChangeSlider action) state = do
  let finalVal = if action then min state.props.sliderMaxValue (state.props.sliderVal + state.props.incrementUnit) else max state.props.sliderMinValue (state.props.sliderVal - state.props.incrementUnit)
  continue state{props{sliderVal = finalVal}}

eval EnterDestination state = do
  exit $ GoToEnterDestination state

eval _ state = continue state
