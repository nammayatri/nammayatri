module Screens.TopPriceView.Controller where

import Debug
import Prelude

import Api.Types (SearchRequest(..))
import Constants (progressTime)
import Data.Array (catMaybes, elem, filter, foldl, mapWithIndex, notElem, null, take)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import PrestoDOM (class Loggable, Eval, continue, continueWithCmd, defaultPerformLog, exit, getPushFn)
import Screens.RideRequestPopUp.ScreenData (RideRequestPopUpScreenData, defaultTabs)
import Types (Action(..)) as RideRequestPopUpAction

data ScreenOutput
  = UpdateTimers RideRequestPopUpScreenData

data Action 
  = AppendRequest (Array SearchRequest)
  | UpdateProgress String Int Number
  | UpdateMaxProgress Int SearchRequest Number
  | OnTabClick Int
  | NoAction
  | OnCardChanged Int
  | NotifyExpired (Array String)

instance showAction :: Show Action where
  show _ = "BackClick"

instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog

eval :: Action -> RideRequestPopUpScreenData -> Eval Action ScreenOutput RideRequestPopUpScreenData

eval (UpdateProgress _ time diffTime) state = do
  let
    updatedRequests = map (\item -> item{currentProgress = item.startTime + item.maxProgress - diffTime, prevProgress = item.currentProgress}) state.tabs
    Tuple onGoingRequests expiredRequested = foldl (\(Tuple ong exp) item -> if item.currentProgress > 0.0 then Tuple (ong <> [item]) exp else Tuple ong (exp <> if item.id == Nothing then [] else [item.id])) (Tuple [] []) updatedRequests
    updatedTabs = take 3 $ onGoingRequests <> defaultTabs
    updatedState = state { tabs = updatedTabs, timer = toNumber time, diffTime = diffTime}
  if null expiredRequested then continue updatedState 
    else continueWithCmd updatedState [ do 
          callPopUpAction (RideRequestPopUpAction.NotifyExpired $ catMaybes expiredRequested)
          pure $ NoAction ]
    

eval (UpdateMaxProgress idx (SearchRequest request) time) state = do
  let
    updatedTabs = mapWithIndex (\index item -> if index == idx && item.currentProgress == 0.0 then item { currentProgress =  if idx /= 0 then state.timer + time - state.diffTime else item.currentProgress, maxProgress = time, price = request.baseFare, id = Just request.searchTryId, startTime = state.timer, prevProgress = time} else item) state.tabs
  continue state { tabs = updatedTabs }

eval (AppendRequest searchData) state = exit $ UpdateTimers state{rideRequests = searchData}

eval (NotifyExpired ids) state = do
  let updatedTabs = take 3 $ (filter (\item -> case item.id of 
                                    Just id -> notElem id ids
                                    Nothing -> false) state.tabs) <> defaultTabs
  continue state{tabs= updatedTabs}

eval (OnTabClick idx) state = do
  let
    updatedState = getTabSelected state idx
  continueWithCmd updatedState
    [ (pure NoAction)
    , ( do
          void $ callPopUpAction $ RideRequestPopUpAction.UpdateCarousel idx
          pure NoAction
      )
    ]
eval (OnCardChanged idx) state = do
  let updatedState = getTabSelected state idx
  continue updatedState

eval _ state = continue state

callPopUpAction :: RideRequestPopUpAction.Action -> Effect Unit
callPopUpAction action = do
  push <- getPushFn (Just "RideRequestPopUp") "RideRequestPopUp"
  push action

getTabSelected :: RideRequestPopUpScreenData -> Int -> RideRequestPopUpScreenData
getTabSelected state idx = state { tabs = mapWithIndex (\index item -> if index == idx then item { isSelected = true } else item { isSelected = false }) state.tabs }