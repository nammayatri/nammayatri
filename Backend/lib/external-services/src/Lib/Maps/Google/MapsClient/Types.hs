{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Lib.Maps.Google.MapsClient.Types where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Double.Conversion.Text (toFixed)
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import Kernel.Prelude
import Lib.Maps.Google.PolyLinePoints (PolyLinePoints)
import Servant (FromHttpApiData (parseUrlPiece), ToHttpApiData (toUrlPiece))

data AutoCompleteResp = AutoCompleteResp
  { status :: Text,
    predictions :: [Prediction]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Prediction = Prediction
  { description :: Text,
    place_id :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data GetPlaceDetailsResp = GetPlaceDetailsResp
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
  deriving (Generic, ToJSON, FromJSON, Show, ToSchema)

instance ToHttpApiData LocationS where
  toUrlPiece (LocationS lat' lng') =
    T.concat [toFixed precision lat', ",", toFixed precision lng']
    where
      precision = 6 -- Precision beyond 6 decimal places is ignored.

instance FromHttpApiData LocationS where
  parseUrlPiece param = do
    let latLon = T.splitOn "," param
    case latLon of
      [latText, lonText] -> do
        lat <- left T.pack . eitherDecode . BSL.fromStrict . DT.encodeUtf8 $ latText
        lng <- left T.pack . eitherDecode . BSL.fromStrict . DT.encodeUtf8 $ lonText
        Right LocationS {..}
      _ -> Left "location should contain lat and lon separated by a comma"

data GetPlaceNameResp = GetPlaceNameResp
  { status :: Text,
    results :: [ResultsResp]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data ResultsResp = ResultsResp
  { formatted_address :: Maybe Text,
    address_components :: [AddressResp],
    plus_code :: Maybe PlusCodeResp,
    geometry :: Geometry
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data AddressResp = AddressResp
  { long_name :: Text,
    short_name :: Text,
    types :: [Text]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

newtype PlusCodeResp = PlusCodeResp
  { compound_code :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data DirectionsResp = DirectionsResp
  { routes :: [Route],
    status :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Route = Route
  { bounds :: Bounds,
    legs :: [Leg]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Bounds = Bounds
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
    polyline :: EncodedPointObject,
    start_location :: LocationS,
    travel_mode :: Mode
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype EncodedPointObject = EncodedPointObject {points :: PolyLinePoints}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

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
    status :: Text
  }
  deriving (Generic, ToJSON, FromJSON)

data TextValue = TextValue
  { text :: Text,
    value :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data Place = Location LocationS | Address Text deriving (Show)

instance ToHttpApiData Place where
  toUrlPiece (Location location) = toUrlPiece location
  toUrlPiece (Address address) = address

instance FromHttpApiData Place where
  parseUrlPiece piece = case parseUrlPiece piece of
    Right location -> Right $ Location location
    _ -> Right $ Address piece

instance ToHttpApiData [Place] where
  toUrlPiece latLongList = T.intercalate "|" $ toUrlPiece <$> latLongList

instance FromHttpApiData [Place] where
  parseUrlPiece piece = do
    let places = T.splitOn "|" piece
    forM places parseUrlPiece

data Mode = DRIVING | WALKING | BICYCLING | TRANSIT
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ToHttpApiData Mode where
  toUrlPiece = T.toLower . show

instance FromHttpApiData Mode where
  parseUrlPiece = left T.pack . eitherDecode .(\str -> "\"" <> str <> "\"") . BSL.fromStrict . DT.encodeUtf8 . T.toUpper

data DepartureTime = Now | FutureTime UTCTime

instance ToHttpApiData DepartureTime where
  toUrlPiece Now = "now"
  toUrlPiece (FutureTime time) = show time
