{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.Location where

import Beckn.Types.Core.Address
import Beckn.Types.Core.Scalar
import Beckn.Utils.Example
import Beckn.Utils.JSON
import qualified Beckn.Utils.Schema as Schema
import Data.Default.Class (Default (..))
import Data.OpenApi (ToSchema (declareNamedSchema), genericDeclareNamedSchema)
import Data.Text
import EulerHS.Prelude

data Location = Location
  { gps :: Maybe GPS,
    address :: Maybe Address,
    station_code :: Maybe Text,
    city :: Maybe City,
    country :: Maybe Country,
    circle :: Maybe Circle,
    polygon :: Maybe Text,
    _3dspace :: Maybe Text
  }
  deriving (Generic, Show)

instance ToSchema Location where
  declareNamedSchema = genericDeclareNamedSchema Schema.stripPrefixUnderscoreIfAny

instance FromJSON Location where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Location where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

emptyLocation :: Location
emptyLocation =
  Location
    { gps = Nothing,
      address = Nothing,
      station_code = Nothing,
      city = Nothing,
      country = Nothing,
      circle = Nothing,
      polygon = Nothing,
      _3dspace = Nothing
    }

instance Default Location where
  def = emptyLocation

instance Example Location where
  example =
    emptyLocation
      { gps = example,
        address = example
      }

gpsLocation :: GPS -> Location
gpsLocation gps =
  Location
    { gps = Just gps,
      address = Nothing,
      station_code = Nothing,
      city = Nothing,
      country = Nothing,
      circle = Nothing,
      polygon = Nothing,
      _3dspace = Nothing
    }

data GPS = GPS
  { lat :: Text,
    lon :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

instance Example GPS where
  example =
    GPS
      { lat = "20.5937",
        lon = "78.9629"
      }

-- Can we add district and state in Address?
data City = City
  { name :: Text,
    code :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data Country = Country
  { name :: Text,
    code :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data Circle = Circle
  { lat :: Text,
    long :: Text,
    radius :: Scalar
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
