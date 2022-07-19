module Beckn.Types.Core.Taxi.Confirm.Location
  ( module Beckn.Types.Core.Taxi.Confirm.Location,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Types.Core.Taxi.Common.Address as Reexport
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)

newtype Location = Location
  { address :: Address
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema Location where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
