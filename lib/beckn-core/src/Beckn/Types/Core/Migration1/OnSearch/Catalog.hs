module Beckn.Types.Core.Migration1.OnSearch.Catalog (Catalog (..)) where

import Beckn.Types.Core.Migration1.OnSearch.Provider (Provider)
import Beckn.Utils.Example
import Beckn.Utils.JSON (slashedRecordFields)
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (exp, id)

newtype Catalog = Catalog
  { bpp_providers :: [Provider]
  }
  deriving (Generic, Show, ToSchema)

instance FromJSON Catalog where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON Catalog where
  toJSON = genericToJSON slashedRecordFields

instance Example Catalog where
  example =
    Catalog
      { bpp_providers = example
      }
