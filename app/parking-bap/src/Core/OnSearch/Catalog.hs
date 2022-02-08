module Core.OnSearch.Catalog (Catalog (..)) where

import Beckn.Prelude hiding (exp)
import Beckn.Utils.JSON (slashedRecordFields)
import Core.Common.Descriptor (Descriptor)
import Core.OnSearch.Provider (Provider)
import Data.OpenApi (ToSchema (..), fromAesonOptions, genericDeclareNamedSchema)

data Catalog = Catalog
  { bpp_descriptor :: Descriptor,
    bpp_providers :: [Provider]
  }
  deriving (Generic, Show)

instance ToSchema Catalog where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions slashedRecordFields

instance FromJSON Catalog where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON Catalog where
  toJSON = genericToJSON slashedRecordFields
