module Components.FavouriteLocationModel.Controller where

import Components.SavedLocationCard.Controller as SavedLocationCardController
import Components.GenericHeader.Controller (Action(..)) as GenericHeaderController
import Components.ErrorModal.Controller as ErrorModalController

data Action = FavouriteLocationAC SavedLocationCardController.Action 
                | GenericHeaderAC GenericHeaderController.Action
                | ErrorModalAC ErrorModalController.Action