{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}


module Screens.DriverSavedLocationScreen.ScreenData where

import Prelude

import Common.Types.App as Common
import Data.Maybe (Maybe(..))
import Screens.Types (DriverSavedLocationScreenState, SavedLocationScreenType(..))
import ConfigProvider

initData :: DriverSavedLocationScreenState
initData =
  { data:
      { address: ""
      , currentLat: Nothing
      , currentLon: Nothing
      , maxGotoLocations : (getAppConfig appConfig).gotoConfig.maxGotoLocations
      , savedLocationsArray: [
        { id : "",
          lat : 0.0,
          lon : 0.0,
          address : "",
          tag : "",
          disabled : false }
      ]
      , predictions: []
      , saveLocationObject:
          { position:
              { place: ""
              , lat: 0.0
              , lon: 0.0
              }
          , address: ""
          , tag: ""
          }
      }
  , props:
      { viewType: GoToList
      , confirmDelete: false
      , selectedLocation: {
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
      , fromEditButton: Nothing
      , gotBackToHomeScreen : false
      , errorText : Nothing
      , defTag : "Home"
      , selectedPrediction:
         { description: ""
            , title: ""
            , placeId: Nothing
            , distance: Nothing
              }
      }
  }
