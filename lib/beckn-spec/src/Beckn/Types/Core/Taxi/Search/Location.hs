module Beckn.Types.Core.Taxi.Search.Location
  ( module Beckn.Types.Core.Taxi.Search.Location,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Address as Reexport
import Beckn.Types.Core.Taxi.Common.Gps as Reexport
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Location = Location
  { gps :: Gps,
    address :: Maybe Address
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance ToSchema Location where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions