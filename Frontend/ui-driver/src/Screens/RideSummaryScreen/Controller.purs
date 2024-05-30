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
import Components.SourceToDestination.Controller as SourceToDestinationController


instance showAction  ::  Show Action where
   show _ = ""
instance loggableAction :: Loggable Action where
  performLog = defaultPerformLog



data Action = BackPressed
              | TermsConditionOpen 
              | ExcludedChargesOpen 
              | IncludedChargesOpen 
              | PickUpOpen
              | SourceToDestinationActionController SourceToDestinationController.Action
              | NoAction

data ScreenOutput = GoBack

eval ::  Action -> RideSummaryScreenState -> Eval Action ScreenOutput RideSummaryScreenState
eval TermsConditionOpen state = do
                  let old = state.props.termsAndConditionOpen
                  continue state{ props{termsAndConditionOpen = not old}}
eval ExcludedChargesOpen state = do
                  let old = state.props.excludedChargesOpen
                  continue state{ props{excludedChargesOpen = not old}}
eval IncludedChargesOpen state = do
                  let old = state.props.includedChargesOpen
                  continue state{ props{includedChargesOpen = not old}}
eval PickUpOpen state = do
                  let old = state.props.pickUpOpen
                  continue state{ props{pickUpOpen = not old}}
eval _ state = continue state
