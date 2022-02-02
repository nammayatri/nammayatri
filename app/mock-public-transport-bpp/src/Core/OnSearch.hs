module Core.OnSearch where

import Beckn.Types.Core.Migration.Image (Image (..))
import Beckn.Utils.JSON (slashedRecordFields)
import Core.Descriptor
import Core.Location
import Core.OnSearch.Departure
import Core.OnSearch.Fare
import Core.OnSearch.Route
import Data.Aeson
import Relude hiding (id)
import Core.OnSearch.Item

newtype OnSearchCatalog = OnSearchCatalog
  { catalog :: Catalog
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Catalog = Catalog
  { bpp_descriptor :: Descriptor,
    bpp_providers :: [Provider]
  }
  deriving (Generic, Show)

instance FromJSON Catalog where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON Catalog where
  toJSON = genericToJSON slashedRecordFields

data Descriptor = Descriptor
  { name :: Text,
    code :: Text,
    symbol :: Text,
    short_desc :: Text,
    long_desc :: Text,
    images :: [Image]
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

data Provider = Provider
  { id :: Text,
    descriptor :: DescriptorId,
    -- categories?
    locations :: [LocationDetails],
    routes :: [Route],
    fares :: [Fare],
    departures :: [Departure],
    items :: [Item]
  }
  deriving (Generic, FromJSON, Show, ToJSON)
