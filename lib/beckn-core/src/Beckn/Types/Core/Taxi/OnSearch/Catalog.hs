module Beckn.Types.Core.Taxi.OnSearch.Catalog (Catalog (..)) where

import Beckn.Types.Core.Taxi.OnSearch.Provider (Provider)
import Beckn.Utils.Example
import Beckn.Utils.JSON (slashedRecordFields)
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (exp, id)

newtype Catalog = Catalog
  { bpp_providers :: [Provider]
  }
  deriving (Generic, Show)

instance ToSchema Catalog where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance FromJSON Catalog where
  parseJSON = genericParseJSON slashedRecordFields

instance ToJSON Catalog where
  toJSON = genericToJSON slashedRecordFields

instance Example Catalog where
  example =
    Catalog
      { bpp_providers = example
      }
