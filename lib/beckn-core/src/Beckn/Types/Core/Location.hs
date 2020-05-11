{-# LANGUAGE DuplicateRecordFields #-}
module Beckn.Types.Core.Location where

import Beckn.Types.Core.Scalar
import Data.Text
import Data.Generics.Labels
import EulerHS.Prelude

data Location = Location
  { _type :: Text, --""gps","address","station_code","area_code","city","country","circle","polygon","3dspace"
    _gps :: Maybe GPS,
    _address :: Maybe Address,
    _station_code :: Maybe Text,
    _area_code :: Maybe Text,
    _city :: Maybe City,
    _country :: Maybe Country,
    _circle :: Maybe Circle,
    _polygon :: Maybe Text,
    _3dspace :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Location where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Location where
  toJSON = genericToJSON stripAllLensPrefixOptions

data GPS = GPS
  { lat :: Double,
    lon :: Double
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data Address = Address
  { door :: Text,
    building :: Text,
    street :: Text,
    area :: Text,
    city :: Text,
    country :: Text,
    area_code :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data City = City
  { name :: Text,
    code :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data Country = Country
  { name :: Text,
    code :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data Circle = Circle
  { lat :: Text,
    long :: Text,
    radius :: Scalar
  }
  deriving (Generic, Show, FromJSON, ToJSON)
