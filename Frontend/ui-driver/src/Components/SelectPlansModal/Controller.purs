module Components.SelectPlansModal.Controller where

import Components.PlanCard.Controller as PlanCardController
import Components.PrimaryButton as PrimaryButton
import Screens.Types as ST
import Data.Maybe
import Prelude (class Show, show, (<>))

instance showAction :: Show Action where
  show (NoAction) = "NoAction"
  show (PlanCardAction var1 var2) = "PlanCardAction_" <> show var1 <> "_" <> show var2
  show (PrimaryButtonAC var1) = "PrimaryButtonAC_" <> show var1
  show (Dismiss) = "Dismiss"
  show (Support) = "Support"

data Action = 
    NoAction 
    | PlanCardAction ST.PlanCardState PlanCardController.Action
    | PrimaryButtonAC PrimaryButton.Action
    | Dismiss
    | Support

type SelectPlansState = {
    plansList :: Array ST.PlanCardState,
    selectedPlan :: Maybe ST.PlanCardState
}

config :: SelectPlansState
config = {
    plansList: [],
    selectedPlan: Nothing
}