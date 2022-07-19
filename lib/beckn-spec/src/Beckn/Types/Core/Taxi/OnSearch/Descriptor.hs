module Beckn.Types.Core.Taxi.OnSearch.Descriptor (Descriptor (..)) where

import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (exp, id)

newtype Descriptor = Descriptor
  { name :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Descriptor where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
