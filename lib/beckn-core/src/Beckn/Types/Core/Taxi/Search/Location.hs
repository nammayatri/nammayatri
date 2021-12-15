module Beckn.Types.Core.Taxi.Search.Location
  ( module Beckn.Types.Core.Taxi.Search.Location,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Gps as Reexport
import Beckn.Types.Core.Taxi.Search.Address (Address)
import Beckn.Utils.Example
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude

data Location = Location
  { gps :: Gps,
    address :: Address
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema Location where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance Example Location where
  example =
    Location
      { gps = example,
        address = example
      }
