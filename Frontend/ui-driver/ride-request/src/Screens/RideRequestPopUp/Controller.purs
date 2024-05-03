module Screens.RideRequestPopUp.Controller where

import Prelude
import PrestoDOM

import Api.Types (SearchRequest(..))
import Data.Maybe (Maybe(..))
import Screens.RideRequestPopUp.ScreenData (RideRequestPopUpScreenData)
import Screens.RideRequestPopUp.TransFormer (toPopupProp)
import Screens.TopPriceView.Controller as TopPriceView

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
  | UpdateRideRequest (Array SearchRequest)
  | NoAction

instance showAction :: Show Action where
  show _ = "BackClick"

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

eval :: Action -> RideRequestPopUpScreenData -> Eval Action ScreenOutput RideRequestPopUpScreenData

eval (UpdateRideRequest searchData) state = continueWithCmd state{holderData = toPopupProp searchData, rideRequests = searchData} [do 
  push <- getPushFn (Just "TopPriceView") "TopPriceView"
  void $ push $ TopPriceView.UpdateRideRequest searchData
  pure NoAction]

eval NextClick state = exit $ NextScreen state

eval BackClick state = exit $ Back state

eval NoAction state = continue state

eval (Decline idx) state = exit $ Back state
