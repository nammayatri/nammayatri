module Screens.TopPriceView.Controller where

import Prelude
import PrestoDOM

import Api.Types (SearchRequest(..))
import Screens.TopPriceView.ScreenData (TopPriceViewState)

-- Controller
-- All actions which can be performed on the screen. Sample click includes NextClick and BackClick.
-- P.S. This is not the actual logic for going to next screen or previous screen. This is just a example
-- for showing 2 kinds of exits from the screen.
data ScreenOutput
  = NextScreen TopPriceViewState
  | Back TopPriceViewState

data Action
  = NextClick
  | UpdateRideRequest (Array SearchRequest)
  | UpdateProgress Int String String Number Int

instance showAction :: Show Action where
  show _ = "BackClick"

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

eval :: Action -> TopPriceViewState -> Eval Action ScreenOutput TopPriceViewState

eval NextClick state = exit $ NextScreen state

eval (UpdateProgress seconds status timerID diffTime idx) state = continue state{progress=diffTime}

eval (UpdateRideRequest searchData) state = continue state{ rideRequests = searchData}
