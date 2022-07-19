module Beckn.Types.Core.Taxi.OnSelect.Category
  ( module Beckn.Types.Core.Taxi.OnSelect.Category,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Types.Core.Taxi.Common.FareProductType as Reexport
import Beckn.Types.Core.Taxi.OnSelect.Descriptor
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)

data Category = Category
  { id :: FareProductType,
    descriptor :: Descriptor
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Category where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
