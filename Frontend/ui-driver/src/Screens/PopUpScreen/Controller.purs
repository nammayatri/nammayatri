{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.PopUpScreen.Controller where

import Prelude
import PrestoDOM
import Screens.Types (PopUpScreenState, Rides, AllocationData)
import Language.Strings(getString)
import Language.Types (STR(..))
import Components.RideAllocationModal.Controller as RideAllocationModal
import Data.Array as Array
import Helpers.Utils (clearPopUpTimer, toStringJSON, secondsLeft, objectToAllocationType, clearAllTimer)
import Data.Maybe
import Log
import JBridge (deletePopUpCallBack)
import Effect.Class (liftEffect)
import Data.String (take)
import Screens (ScreenName(..), getScreen)

instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = case action of
      AfterRender -> trackAppScreenRender appId "screen" (getScreen POPUP_SCREEEN)
      BackPressed -> do
        trackAppBackPress appId (getScreen POPUP_SCREEEN)
        trackAppEndScreen appId (getScreen POPUP_SCREEEN)
      PopUpCallBack stringifyPayload ->trackAppScreenEvent appId (getScreen POPUP_SCREEEN) "in_screen" "popup_callback"
      RideAllocationModalAction act -> case act of
        RideAllocationModal.Decline id -> trackAppActionClick appId (getScreen POPUP_SCREEEN) "ride_allocation_modal" "decline_onclick"
        RideAllocationModal.Request id extraFare -> trackAppActionClick appId (getScreen POPUP_SCREEEN) "ride_allocation_modal" "request_onclick"
        RideAllocationModal.IncreasePrice id -> trackAppActionClick appId (getScreen POPUP_SCREEEN) "ride_allocation_modal" "increase_price_onclick"
        RideAllocationModal.DecreasePrice id -> trackAppActionClick appId (getScreen POPUP_SCREEEN) "ride_allocation_modal" "decrease_price_onclick"
        RideAllocationModal.CountDown seconds id status timerID -> trackAppScreenEvent appId (getScreen POPUP_SCREEEN) "ride_allocation_modal" "countdown"
        RideAllocationModal.NoAction -> trackAppActionClick appId (getScreen POPUP_SCREEEN) "ride_allocation_modal" "no_action"
      NoAction -> pure unit

data ScreenOutput =  GoBack | RequestRide String Number

data Action = BackPressed
            | NoAction
            | PopUpCallBack String 
            | RideAllocationModalAction RideAllocationModal.Action
            | AfterRender

eval :: Action -> PopUpScreenState -> Eval Action ScreenOutput PopUpScreenState
eval BackPressed state = continue state
eval (RideAllocationModalAction (RideAllocationModal.Decline id)) state = do 
    let rides = Array.filter (\ride -> ride.id /= id ) state.data.availableRides
    if (Array.length rides) > 0 then do 
        let newState = state { data = state.data { availableRides = rides } } 
        continue newState
        else do 
            _ <- pure $ deletePopUpCallBack ""
            exit $ GoBack
eval (RideAllocationModalAction (RideAllocationModal.Request id extraFare)) state = do
  _ <- pure $ deletePopUpCallBack ""
  _ <- pure $ clearAllTimer ""
  exit $ RequestRide id extraFare
eval (RideAllocationModalAction (RideAllocationModal.CountDown seconds id status timerID)) state = do 
  _ <- pure $ printLog "rideAllocationModal state" state
  if status == "EXPIRED" then do
    _ <- pure $ printLog "id EXPIRED" id
    _ <- pure $ printLog "TimerID EXPIRED" timerID
    _ <- pure $ clearPopUpTimer timerID
    let rides = Array.filter (\ride -> ride.id /= id ) state.data.availableRides
    if (Array.length rides) > 0 then do 
        let newState = state { data = state.data { availableRides = rides } } 
        continue newState
        else do 
            _ <- pure $ printLog "calling deletePopUpCallBack" "."
            _ <- pure $ deletePopUpCallBack ""
            exit $ GoBack
  else do
    let newState = state { data = state.data { availableRides = map (\ride -> if ride.id == id then ride { timer = seconds } else ride ) state.data.availableRides } }
    continue newState
eval (RideAllocationModalAction (RideAllocationModal.IncreasePrice id)) state = do 
  let newState = state { data = state.data { availableRides = map (\ride -> if ride.id == id then ride { totalAmount = ride.totalAmount + ride.increasePrice } else ride ) state.data.availableRides } }
  continue newState
eval (RideAllocationModalAction (RideAllocationModal.DecreasePrice id)) state = do 
  let newState = state { data = state.data { availableRides = map (\ride -> if ride.id == id then ride { totalAmount = ride.totalAmount - ride.decreasePrice } else ride ) state.data.availableRides } }
  continue newState
eval (PopUpCallBack stringifyPayload) state = do 
  _ <- pure $ printLog "stringify payload" stringifyPayload
  let entityPayload = (objectToAllocationType stringifyPayload)
  _ <- pure $ printLog "object payload" entityPayload

  let availableRide = (transformAllocationData entityPayload)
  continue state { data { availableRides = state.data.availableRides <> availableRide } }
eval _ state = continue state

transformAllocationData :: AllocationData -> Array Rides 
transformAllocationData ride = [{
  id : ride.searchRequestId,
  timer : 0,
  seconds : if ((secondsLeft ride.searchRequestValidTill) > 30) then 30 else (secondsLeft ride.searchRequestValidTill),
  pickupDistance : (ride.distanceToPickup / 1000),
  journeyDistance : (ride.distance / 1000),
  sourceAddress : ride.fromLocation.full_address,
  destinationAddress : ride.toLocation.full_address,
  totalAmount : ride.baseFare,
  baseAmount : ride.baseFare,
  increasePrice : 10.0,
  decreasePrice : 10.0,
  destinationArea : if ride.toLocation.area /= "" then ride.toLocation.area else ""
}]