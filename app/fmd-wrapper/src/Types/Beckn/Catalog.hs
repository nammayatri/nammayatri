module Types.Beckn.Catalog (Catalog (..)) where

import Beckn.Utils.JSON (slashedRecordFields)
import Data.OpenApi (ToSchema (..), fromAesonOptions, genericDeclareNamedSchema)
import EulerHS.Prelude hiding (id)
import Types.Beckn.Provider (Provider)

newtype Catalog = Catalog
  { bpp_providers :: [Provider]
  }
  deriving (Generic, Show)

instance ToSchema Catalog where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions slashedRecordFields

instance FromJSON Catalog where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON Catalog where
  toJSON = genericToJSON slashedRecordFields
