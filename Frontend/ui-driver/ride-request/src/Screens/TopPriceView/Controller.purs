module Screens.TopPriceView.Controller where

import Prelude
import PrestoDOM

import Api.Types (SearchRequest(..))
import Data.Array (mapWithIndex, nub)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Screens.TopPriceView.ScreenData (TopPriceViewState)
import Types (Action(..)) as RideRequestPopUpAction
import Web.DOM.DOMTokenList (item)

-- Controller
-- All actions which can be performed on the screen. Sample click includes NextClick and BackClick.
-- P.S. This is not the actual logic for going to next screen or previous screen. This is just a example
-- for showing 2 kinds of exits from the screen.
data ScreenOutput
  = UpdateTimers TopPriceViewState

data Action
  = UpdateRideRequest (Array SearchRequest)
  | AppendRequest (Array SearchRequest)
  | UpdateProgress String Int Number
  | UpdateMaxProgress Int SearchRequest Number
  | OnTabClick Int
  | NoAction

instance showAction :: Show Action where
  show _ = "BackClick"

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

eval :: Action -> TopPriceViewState -> Eval Action ScreenOutput TopPriceViewState

eval (UpdateProgress _ time diffTime) state = do
  let
    updatedTabs = map (\item -> item{currentProgress = item.startTime + item.maxProgress - diffTime}) state.tabs
  continue state { tabs = updatedTabs, timer = diffTime}

eval (UpdateMaxProgress idx (SearchRequest request) time) state = do
  let
    updatedTabs = mapWithIndex (\index item -> if index == idx then item { maxProgress = time, price = request.baseFare, id = Just request.searchTryId, startTime = state.timer } else item) state.tabs
  continue state { tabs = updatedTabs }

eval (UpdateRideRequest searchData) state = exit $ UpdateTimers state{rideRequests = searchData}
eval (AppendRequest searchData) state = exit $ UpdateTimers state{rideRequests = searchData}

eval (OnTabClick idx) state = do
  let
    updatedState = state { tabs = mapWithIndex (\index item -> if index == idx then item { selected = true } else item { selected = false }) state.tabs }
  continueWithCmd updatedState
    [ (pure NoAction)
    , ( do
          void $ callPopUpAction RideRequestPopUpAction.NoAction
          pure NoAction
      )
    ]

eval _ state = continue state

callPopUpAction :: RideRequestPopUpAction.Action -> Effect Unit
callPopUpAction action = do
  push <- getPushFn (Just "RideRequestPopUp") "RideRequestPopUp"
  push action
