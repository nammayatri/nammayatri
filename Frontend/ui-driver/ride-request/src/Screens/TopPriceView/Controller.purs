module Screens.TopPriceView.Controller where

import Prelude
import PrestoDOM

import Api.Types (SearchRequest(..))
import Data.Array (catMaybes, filter, foldr, mapWithIndex, nub, null, take)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Debug (spy)
import Effect (Effect)
import Screens.RideRequestPopUp.ScreenData (RideRequestPopUpScreenData, defaultTabs)
import Types (Action(..)) as RideRequestPopUpAction
import Web.DOM.DOMTokenList (item)

-- Controller
-- All actions which can be performed on the screen. Sample click includes NextClick and BackClick.
-- P.S. This is not the actual logic for going to next screen or previous screen. This is just a example
-- for showing 2 kinds of exits from the screen.
data ScreenOutput
  = UpdateTimers RideRequestPopUpScreenData

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

eval :: Action -> RideRequestPopUpScreenData -> Eval Action ScreenOutput RideRequestPopUpScreenData

eval (UpdateProgress _ time diffTime) state = do
  let
    updatedRequests = map (\item -> item{currentProgress = item.startTime + item.maxProgress - diffTime}) state.tabs
    Tuple onGoingRequests expiredRequested = foldr (\item (Tuple ong exp) -> if item.currentProgress > 0.0 then Tuple (ong <> [item]) exp else Tuple ong (exp <> if item.id == Nothing then [] else [item.id])) (Tuple [] []) updatedRequests
    updatedTabs = take 3 $ onGoingRequests <> defaultTabs
    updatedState = state { tabs = updatedTabs, timer = toNumber time}
  if null expiredRequested then continue updatedState 
    else continueWithCmd updatedState [ do 
          callPopUpAction (RideRequestPopUpAction.NotifyExpired $ catMaybes expiredRequested)
          pure $ NoAction ]
    

eval (UpdateMaxProgress idx (SearchRequest request) time) state = do
  let
    updatedTabs = mapWithIndex (\index item -> if index == idx then item { maxProgress = time, price = request.baseFare, id = Just request.searchTryId, startTime = state.timer, isSelected = true} else item) state.tabs
  continue state { tabs = updatedTabs }

eval (UpdateRideRequest searchData) state = exit $ UpdateTimers state{rideRequests = searchData}
eval (AppendRequest searchData) state = exit $ UpdateTimers state{rideRequests = searchData}

eval (OnTabClick idx) state = do
  let
    updatedState = state { tabs = mapWithIndex (\index item -> if index == idx then item { isSelected = true } else item { isSelected = false }) state.tabs }
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
