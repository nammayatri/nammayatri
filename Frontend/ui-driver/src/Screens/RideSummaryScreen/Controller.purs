module Screens.RideSummaryScreen.Controller where

import Prelude
import Screens.RideSummaryScreen.ScreenData
import Data.Array (find, elem, filter, mapWithIndex, length)
import PrestoDOM (Eval, continue, exit, continueWithCmd , update)
import PrestoDOM.Types.Core (class Loggable, defaultPerformLog)
import Prim.Boolean (False)
import Screens (ScreenName(..), getScreen)
import Data.Function.Uncurried (runFn1)
import Engineering.Helpers.Commons as EHC
import JBridge as JBridge
import Debug
import PrestoDOM.Types.Core (class Loggable, toPropValue)


instance showAction  ::  Show Action where
   show _ = ""
instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog



data Action = BackPressed
              | TermOpen 
              | ExChargeOpen 
              | InChargeOpen 

data ScreenOutput = GoBack

eval ::  Action -> RideSummaryScreenState -> Eval Action ScreenOutput RideSummaryScreenState
eval TermOpen state = do
                  let old = state.props.termsOpen
                  continue state{ props{termsOpen = not old}}
eval ExChargeOpen state = do
                  let old = state.props.exChargesOpen
                  continue state{ props{exChargesOpen = not old}}
eval InChargeOpen state = do
                  let old = state.props.inChargesOpen
                  continue state{ props{inChargesOpen = not old}}
eval _ state = continue state
