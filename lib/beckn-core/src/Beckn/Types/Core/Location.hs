{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Location where

import Beckn.Types.Core.Scalar
import Beckn.Utils.Common
import Data.Text
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

instance Example Location where
  example =
    Location
      { _type = "gps",
        _gps = example,
        _address = Nothing,
        _station_code = Nothing,
        _area_code = Nothing,
        _city = Nothing,
        _country = Nothing,
        _circle = Nothing,
        _polygon = Nothing,
        _3dspace = Nothing
      }

data GPS = GPS
  { lat :: Text,
    lon :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Example GPS where
  example =
    GPS
      { lat = "20.5937",
        lon = "78.9629"
      }

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

instance Example Address where
  example =
    Address
      { door = "#817",
        building = "Juspay Apartments",
        street = "27th Main",
        area = "8th Block Koramangala",
        city = "Bangalore",
        country = "India",
        area_code = "560047"
      }

-- Can we add district and state in Address?
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
