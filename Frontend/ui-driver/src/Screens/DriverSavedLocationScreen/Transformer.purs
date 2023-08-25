module Screens.DriverSavedLocationScreen.Transformer where

import Prelude

import Screens.Types (GoToLocation)
import Services.API (DriverHomeLocationAPIEntity(..), GetHomeLocationsRes(..))

getLocationArray :: GetHomeLocationsRes -> Array GoToLocation
getLocationArray (GetHomeLocationsRes resp) = 
    let locations = resp.locations
    in map (\ (DriverHomeLocationAPIEntity entity) -> {
        id : entity.id,
        lat : entity.lat,
        lon : entity.lon,
        address : entity.address,
        tag : entity.tag
        }
    ) locations