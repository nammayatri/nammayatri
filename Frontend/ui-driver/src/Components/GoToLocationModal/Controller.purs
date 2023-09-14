{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}


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
  editAcText :: Maybe String,
  disabled :: Boolean
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
  editAcText : Nothing,
  disabled : false
}