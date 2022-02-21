module Core.Spec.Common.Quotation where

import Beckn.Prelude
import Beckn.Utils.GenericPretty (PrettyShow)
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.Spec.Common.Duration
import Core.Spec.Common.Price
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data Quotation = Quotation
  { price :: Price,
    breakup :: [BreakupItem],
    ttl :: Maybe Duration
  }
  deriving (Generic, Show, ToJSON, FromJSON, PrettyShow)

instance ToSchema Quotation where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data BreakupItem = BreakupItem
  { title :: Text,
    price :: Price
  }
  deriving (Generic, Show, ToJSON, FromJSON, PrettyShow)

instance ToSchema BreakupItem where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
