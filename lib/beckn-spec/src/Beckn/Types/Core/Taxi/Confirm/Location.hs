module Beckn.Types.Core.Taxi.Confirm.Location
  ( module Beckn.Types.Core.Taxi.Confirm.Location,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Address as Reexport
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import Kernel.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

newtype Location = Location
  { address :: Address
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema Location where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
