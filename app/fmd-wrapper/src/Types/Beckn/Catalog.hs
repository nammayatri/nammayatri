module Types.Beckn.Catalog (Catalog (..)) where

import Beckn.Utils.JSON (slashedRecordFields)
import EulerHS.Prelude hiding (id)
import Types.Beckn.Provider (Provider)

newtype Catalog = Catalog
  { bpp_providers :: [Provider]
  }
  deriving (Generic, Show)

instance FromJSON Catalog where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON Catalog where
  toJSON = genericToJSON slashedRecordFields
