module Beckn.External.GoogleMaps.Types where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude

data SearchLocationResp = SearchLocationResp
  { status :: Text,
    predictions :: [Prediction]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Prediction = Prediction
  { description :: Text,
    place_id :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data PlaceDetailsResp = PlaceDetailsResp
  { status :: Text,
    result :: PlaceDetailsResult
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype PlaceDetailsResult = PlaceDetailsResult
  { geometry :: Geometry
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype Geometry = Geometry
  { location :: LocationS
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data LocationS = LocationS
  { lat :: Double,
    lng :: Double
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data GetPlaceNameResp = GetPlaceNameResp
  { status :: Text,
    results :: [ResultsResp]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype ResultsResp = ResultsResp
  { formatted_address :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
