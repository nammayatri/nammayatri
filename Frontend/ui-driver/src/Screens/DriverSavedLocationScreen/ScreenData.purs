module Screens.DriverSavedLocationScreen.ScreenData where

import Prelude
import Common.Types.App as Common
import Data.Maybe (Maybe(..))
import Screens.Types (DriverSavedLocationScreenState, SavedLocationScreenType(..))
import Services.API (Prediction(..))

initData :: DriverSavedLocationScreenState
initData =
  { data:
      { address: ""
      , lat: 0.0
      , lon: 0.0
      , savedLocationsArray: []
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
      { viewType: GO_TO_LIST
      , confirmDelete: false
      , selectedLocation: ""
      , fromEditButton: false
      , selectedPrediction:
          Prediction
            $ { description: "String"
              , placeId: Nothing
              , distance: Nothing
              }
      }
  }
