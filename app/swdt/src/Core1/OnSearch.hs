module Core1.OnSearch where

import Beckn.Prelude hiding (exp)
import Beckn.Utils.JSON (slashedRecordFields)
import Core1.Descriptor
import Core1.Provider

newtype OnSearchCatalog = OnSearchCatalog
  { catalog :: Catalog
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

data Catalog = Catalog
  { bpp_descriptor :: DescriptorDetails,
    bpp_providers :: [Provider]
  }
  deriving (Generic)

instance FromJSON Catalog where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON Catalog where
  toJSON = genericToJSON slashedRecordFields
