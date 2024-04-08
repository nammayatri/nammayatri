module Screens.RideRequestPopUp.Controller where

import Prelude
import PrestoDOM
import Screens.RideRequestPopUp.ScreenData (RideRequestPopUpScreenData)

-- Controller
-- All actions which can be performed on the screen. Sample click includes NextClick and BackClick.
-- P.S. This is not the actual logic for going to next screen or previous screen. This is just a example
-- for showing 2 kinds of exits from the screen.
data ScreenOutput
  = NextScreen RideRequestPopUpScreenData
  | Back RideRequestPopUpScreenData

data Action
  = NextClick
  | BackClick
  | Decline Int

instance showAction :: Show Action where
  show _ = "BackClick"

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

eval :: Action -> RideRequestPopUpScreenData -> Eval Action ScreenOutput RideRequestPopUpScreenData
eval NextClick state = exit $ NextScreen state

eval BackClick state = exit $ Back state

eval (Decline idx) state = exit $ Back state
