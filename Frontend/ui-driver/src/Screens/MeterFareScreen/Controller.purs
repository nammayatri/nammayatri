module Screens.MeterFareScreen.Controller where

import JBridge as JB
import Prelude 
import PrestoDOM 
import Screens.Types as ST

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog _ _ = pure unit

data Action
  = BackPressed
  | NoAction

data ScreenOutput
  = GoToHomeScreen ST.MeterFareScreenState

eval :: Action -> ST.MeterFareScreenState -> Eval Action ScreenOutput ST.MeterFareScreenState

eval BackPressed state = do
  void $ pure $ JB.hideKeyboardOnNavigation true
  exit $ GoToHomeScreen state

eval _ state = update state
