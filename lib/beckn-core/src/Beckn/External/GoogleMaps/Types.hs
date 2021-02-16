module Beckn.External.GoogleMaps.Types where

import EulerHS.Prelude

data SearchLocationResp = SearchLocationResp
  { status :: Text,
    predictions :: [Prediction]
  }
  deriving (Generic, ToJSON, FromJSON)

data Prediction = Prediction
  { description :: Text,
    place_id :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON)

data PlaceDetailsResp = PlaceDetailsResp
  { status :: Text,
    result :: PlaceDetailsResult
  }
  deriving (Generic, ToJSON, FromJSON)

newtype PlaceDetailsResult = PlaceDetailsResult
  { geometry :: Geometry
  }
  deriving (Generic, ToJSON, FromJSON)

newtype Geometry = Geometry
  { location :: LocationS
  }
  deriving (Generic, ToJSON, FromJSON)

data LocationS = LocationS
  { lat :: Double,
    lng :: Double
  }
  deriving (Generic, ToJSON, FromJSON)

newtype GetPlaceNameResp = GetPlaceNameResp
  { results :: [ResultsResp]
  }
  deriving (Generic, ToJSON, FromJSON)

newtype ResultsResp = ResultsResp
  { formatted_address :: Text
  }
  deriving (Generic, ToJSON, FromJSON)
