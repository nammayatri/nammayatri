module Components.GoToLocationModal.Controller where


import Data.Maybe (Maybe(..))

data Action = EditLocation GoToModalConfig
            | DeleteLocation GoToModalConfig
            | CardClicked GoToModalConfig
            | NoAction 

type GoToModalConfig = {
  id :: String,
  lat :: Number,
  lon :: Number,
  address :: String,
  tag :: String,
  isSelectable :: Boolean,
  isEditEnabled :: Boolean,
  isSelected :: Boolean,
  removeAcText :: Maybe String,
  editAcText :: Maybe String
}

config :: GoToModalConfig
config = {
  id : "",
  lat : 0.0,
  lon : 0.0,
  address : "",
  tag : "",
  isSelectable : false,
  isEditEnabled : false,
  isSelected : false,
  removeAcText : Nothing,
  editAcText : Nothing
}