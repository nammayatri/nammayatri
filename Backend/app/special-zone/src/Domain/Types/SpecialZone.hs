{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.SpecialZone where

import Data.Aeson
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.JSON

data SpecialZone = SpecialZone
  { id :: Id SpecialZone,
    name :: Text,
    categoryCode :: Category,
    geoJson :: ShapeFile,
    city :: Text,
    state :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

data SpecialZoneAPIEntity = SpecialZoneAPIEntity
  { name :: Text,
    categoryCode :: Category,
    geoJson :: ShapeFile,
    city :: Text,
    state :: Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

data EntryExit = EntryExit
  { id :: Id EntryExit,
    specialZoneId :: Id SpecialZone,
    _type :: EntryExitType,
    lat :: Double,
    lon :: Double,
    area :: Maybe Text,
    address :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)

data EntryExitType = Entry | Exit deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema, Read)

data Category
  = Metro
  | Airport
  | School
  | Hospital
  | Station
  deriving (Read, Show, Generic, Eq, FromJSON, ToJSON, ToSchema)

newtype ShapeFile = ShapeFile ShapeFileType deriving (Generic, Show, Eq, ToSchema, Read)

instance ToJSON ShapeFile where
  toJSON (ShapeFile shapeData) = toJSON shapeData

instance FromJSON ShapeFile where
  parseJSON a@(Object v) = do
    _type :: Text <- v .: "type"
    case _type of
      "FeatureCollection" -> ShapeFile <$> parseJSON a
      _ -> fail "Unknown type"
  parseJSON _ = fail "Invalid ShapeFile"

data ShapeFileType = ShapeFileType
  { _type :: Text,
    features :: [LocationFeature]
  }
  deriving (Generic, Show, Eq, ToSchema, Read)

instance FromJSON ShapeFileType where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON ShapeFileType where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data LocationFeature = LocationFeature
  { _type :: Text,
    properties :: SzType,
    geometry :: Geometry
  }
  deriving (Generic, Show, Eq, ToSchema, Read)

instance FromJSON LocationFeature where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON LocationFeature where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data Geometry = Geometry
  { _type :: GeometryType,
    coordinates :: Coordinates
  }
  deriving (Generic, Show, Eq, ToSchema, Read)

data Coordinates = MPolyCoords [[[[Double]]]] | PolyCoords [[[Double]]] | PointCoords [Double] deriving (Generic, Show, Eq, ToSchema, Read)

data GeometryType = MultiPolygon | Polygon | Point deriving (Generic, Show, Eq, ToSchema, Read, FromJSON, ToJSON)

instance ToJSON Coordinates where
  toJSON (MPolyCoords mpData) = toJSON mpData
  toJSON (PolyCoords pData) = toJSON pData
  toJSON (PointCoords pointData) = toJSON pointData

instance FromJSON Coordinates where
  parseJSON = genericParseJSON untaggedValue

instance FromJSON Geometry where
  parseJSON (Object v) = do
    type_ :: Text <- v .: "type"
    case type_ of
      "MultiPolygon" -> do
        coordinates_ <- v .: "coordinates"
        return Geometry {_type = MultiPolygon, coordinates = MPolyCoords coordinates_}
      "Polygon" -> do
        coordinates_ <- v .: "coordinates"
        return Geometry {_type = MultiPolygon, coordinates = MPolyCoords [coordinates_]}
      _ -> do
        coordinates_ <- v .: "coordinates"
        return Geometry {_type = Point, coordinates = PointCoords coordinates_}
  parseJSON _ = fail "Invalid Geometry"

instance ToJSON Geometry where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

data SzType = SzType
  { _type :: Maybe EntryExitType,
    area :: Maybe Text,
    address :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToSchema, Read)

instance FromJSON SzType where
  parseJSON = withObject "SzType" $ \v -> do
    _type <- v .:? "type"
    area <- v .:? "area"
    address <- v .:? "address"
    return $ SzType _type area address

instance ToJSON SzType where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
