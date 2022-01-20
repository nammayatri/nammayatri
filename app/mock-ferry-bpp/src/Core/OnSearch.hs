module Core.OnSearch where

import Beckn.Prelude hiding (exp)
import Beckn.Types.Core.Migration.Image (Image (..))
import Beckn.Utils.JSON (slashedRecordFields)
import Core.Descriptor
import Core.Fulfillment
import Core.Location
import Core.Price

newtype OnSearchCatalog = OnSearchCatalog
  { catalog :: Catalog
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data Catalog = Catalog
  { bpp_descriptor :: Descriptor,
    bpp_providers :: [Provider]
  }
  deriving (Generic)

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
    fulfillments :: [FullInfoFulfillment],
    locations :: [LocationDetails],
    items :: [Item]
  }
  deriving (Generic, FromJSON, Show, ToJSON)

data Item = Item
  { id :: Text,
    fulfillment_id :: Text,
    descriptor :: DescriptorId,
    price :: Price
  }
  deriving (Generic, Show, ToJSON, FromJSON)
