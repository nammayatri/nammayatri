module Components.SelectVehicleTypeModal.Controller where

import Screens.Types (VehicalTypes)

data Action = OnCloseClick | OnSelect VehicalTypes

type SelectVehicleTypeModalState =
  { title :: String
  , listItems :: Array VehicalTypes
  }