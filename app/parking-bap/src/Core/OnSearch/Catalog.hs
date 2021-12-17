module Core.OnSearch.Catalog (Catalog (..)) where

import Beckn.Prelude hiding (exp)
import Beckn.Utils.JSON (slashedRecordFields)
import Core.OnSearch.Descriptor (Descriptor)
import Core.OnSearch.Provider (Provider)

data Catalog = Catalog
  { bpp_descriptor :: Descriptor,
    bpp_providers :: [Provider]
  }
  deriving (Generic)

instance FromJSON Catalog where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON Catalog where
  toJSON = genericToJSON slashedRecordFields