module Engineering.Helpers.Events where

import Prelude
import Data.Maybe
import Data.Function.Uncurried (Fn2(..))
import Control.Monad.Except.Trans (lift)
import Engineering.Helpers.Commons (liftFlow)
import Engineering.Helpers.BackTrack (liftFlowBT)
import Presto.Core.Types.Language.Flow (Flow)
import Effect (Effect)
import Debug
import Types.App (FlowBT)

------------------------------------------------------------------------------
------------------------- Namma Yatri Event Pipeline -------------------------
------------------------------------------------------------------------------
foreign import initMeasuringDuration :: String -> Effect Unit
foreign import endMeasuringDuration :: String -> Effect Unit
foreign import addEventData :: String -> String -> Effect Unit
foreign import getEvents :: String -> Effect String

measureDuration :: forall a. String -> Effect a -> Effect a
measureDuration name action = do
  void $ initMeasuringDuration name
  result <- action
  void $ endMeasuringDuration name
  pure result

measureDurationFlow :: forall e st. String -> Flow e st -> Flow e st
measureDurationFlow name action = do  
  void $ liftFlow $ initMeasuringDuration name
  result <- action  
  void $ liftFlow $ endMeasuringDuration name
  pure result

measureDurationFlowBT :: forall e st. String -> FlowBT e st -> FlowBT e st
measureDurationFlowBT name action = do  
  liftFlowBT $ initMeasuringDuration name
  result <- action
  liftFlowBT $ endMeasuringDuration name
  pure result