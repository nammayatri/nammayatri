module Beckn.Types.Core.Taxi.OnSearch.Category
  ( module Beckn.Types.Core.Taxi.OnSearch.Category,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.FareProductType as Reexport
import Beckn.Types.Core.Taxi.OnSearch.Descriptor
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import Kernel.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Category = Category
  { id :: FareProductType,
    descriptor :: Descriptor
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Category where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
