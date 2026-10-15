{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}


module Screens.DriverSavedLocationScreen.Transformer where

import Prelude

import Data.Array as DA
import Data.Maybe (isJust)
import Data.String (toLower, trim)
import Screens.Types (GoToLocation)
import Services.API (DriverHomeLocationAPIEntity(..), GetHomeLocationsRes(..))
import Storage as Storage

getLocationArray :: GetHomeLocationsRes -> Array GoToLocation
getLocationArray (GetHomeLocationsRes resp) = 
    let locations = resp.locations
        _ = Storage.setValueToLocalStore Storage.SAVED_GOTO_COUNT $ show $ DA.length locations
    in map (\ (DriverHomeLocationAPIEntity entity) -> {
        id : entity.id,
        lat : entity.lat,
        lon : entity.lon,
        address : entity.address,
        tag : entity.tag,
        disabled : false
        }
    ) locations

tagAlreadySaved :: Array GoToLocation -> String -> Boolean
tagAlreadySaved arr tag = isJust (DA.find (\ item -> (toLower (trim item.tag)) == toLower tag) arr)
