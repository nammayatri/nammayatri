module Components.SelectPlansModal.Controller where

import Components.PlanCard.Controller as PlanCardController
import Components.PrimaryButton as PrimaryButton
import Screens.Types as ST
import Data.Maybe

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