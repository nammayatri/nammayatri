module Beckn.Types.Core.Metro.OnSearch.Catalog where

import Beckn.Types.Core.Metro.OnSearch.Descriptor (Descriptor)
import Beckn.Types.Core.Metro.OnSearch.Provider
import Beckn.Utils.JSON (slashedRecordFields)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (exp, id)

data Catalog = Catalog
  { bpp_descriptor :: Descriptor,
    bpp_providers :: [Provider]
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON Catalog where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON Catalog where
  toJSON = genericToJSON slashedRecordFields

newtype OnSearchCatalog = OnSearchCatalog
  { catalog :: Catalog
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
