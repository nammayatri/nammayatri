module Beckn.Types.Core.Taxi.OnSearch.Descriptor (Descriptor (..)) where

import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (exp, id)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

newtype Descriptor = Descriptor
  { name :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Descriptor where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
