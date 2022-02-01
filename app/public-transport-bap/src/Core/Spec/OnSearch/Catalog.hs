module Core.Spec.OnSearch.Catalog where

import Beckn.Prelude hiding (exp)
import Beckn.Utils.JSON (slashedRecordFields)
import Core.Spec.OnSearch.Descriptor (Descriptor)
import Core.Spec.OnSearch.Provider (Provider)
import Data.OpenApi (ToSchema (..), fromAesonOptions, genericDeclareNamedSchema)

data Catalog = Catalog
  { bpp_descriptor :: Descriptor,
    bpp_providers :: [Provider]
  }
  deriving (Generic)

instance ToSchema Catalog where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions slashedRecordFields

instance FromJSON Catalog where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON Catalog where
  toJSON = genericToJSON slashedRecordFields
