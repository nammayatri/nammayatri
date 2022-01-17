module Core.OnConfirm.SpecQuote where

import Beckn.Prelude
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Core.OnConfirm.Breakup
import Core.OnConfirm.Price
import Data.OpenApi (ToSchema (declareNamedSchema), defaultSchemaOptions)

data SpecQuote = SpecQuote
  { price :: Price,
    breakup :: [Breakup]
  }
  deriving (Generic, FromJSON, ToJSON)

instance ToSchema SpecQuote where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
