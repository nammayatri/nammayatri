{-# LANGUAGE DerivingStrategies #-}

module Beckn.External.GoogleMaps.Types where

import Beckn.Prelude
import Data.Double.Conversion.Text (toFixed)
import qualified Data.Text as T
import Servant

type HasGoogleMaps m r c = (MonadReader r m, HasField "googleMapsUrl" r BaseUrl, HasField "googleMapsKey" r Text)

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
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype Geometry = Geometry
  { location :: LocationS
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LocationS = LocationS
  { lat :: Double,
    lng :: Double
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

instance ToHttpApiData LocationS where
  toUrlPiece (LocationS lat' lng') =
    T.concat [toFixed precision lat', ",", toFixed precision lng']
    where
      precision = 6 -- Precision beyond 6 decimal places is ignored.

data GetPlaceNameResp = GetPlaceNameResp
  { status :: Text,
    results :: [ResultsResp]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype ResultsResp = ResultsResp
  { formatted_address :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DirectionsResp = DirectionsResp
  { routes :: [Route],
    status :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Route = Route
  { bounds :: Bouds,
    legs :: [Leg]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Bouds = Bounds
  { northeast :: LocationS,
    southwest :: LocationS
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Leg = Leg
  { distance :: TextValue,
    duration :: TextValue,
    end_location :: LocationS,
    start_location :: LocationS,
    steps :: [Step]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Step = Step
  { distance :: TextValue,
    duration :: TextValue,
    end_location :: LocationS,
    start_location :: LocationS,
    travel_mode :: Mode
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data DistanceMatrixResp = DistanceMatrixResp
  { destination_addresses :: [Text],
    origin_addresses :: [Text],
    rows :: [DistanceMatrixRow],
    status :: Text
  }
  deriving (Generic, ToJSON, FromJSON)

newtype DistanceMatrixRow = DistanceMatrixRow {elements :: [DistanceMatrixElement]}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data DistanceMatrixElement = DistanceMatrixElement
  { distance :: Maybe TextValue,
    duration :: Maybe TextValue,
    duration_in_traffic :: Maybe TextValue,
    status :: Text
  }
  deriving (Generic, ToJSON, FromJSON)

data TextValue = TextValue
  { text :: Text,
    value :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Place = Location LocationS | Address Text

instance ToHttpApiData Place where
  toUrlPiece (Location location) = toUrlPiece location
  toUrlPiece (Address address) = address

instance ToHttpApiData [Place] where
  toUrlPiece latLongList = T.intercalate "|" $ toUrlPiece <$> latLongList

data Mode = DRIVING | WALKING | BICYCLING | TRANSIT
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance ToHttpApiData Mode where
  toUrlPiece = T.toLower . show

data DepartureTime = Now | FutureTime UTCTime

instance ToHttpApiData DepartureTime where
  toUrlPiece Now = "now"
  toUrlPiece (FutureTime time) = show time
