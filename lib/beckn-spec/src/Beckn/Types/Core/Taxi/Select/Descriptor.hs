module Beckn.Types.Core.Taxi.Select.Descriptor
  ( module Beckn.Types.Core.Taxi.Select.Descriptor,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.ItemCode as Reexport
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (State, id, state)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

newtype Descriptor = Descriptor
  { code :: ItemCode
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Descriptor where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
