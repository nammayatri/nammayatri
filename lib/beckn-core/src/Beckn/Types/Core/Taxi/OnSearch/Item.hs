module Beckn.Types.Core.Taxi.OnSearch.Item
  ( module Beckn.Types.Core.Taxi.OnSearch.Item,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Price as Reexport
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id)

data Item = Item
  { id :: Text,
    vehicle_variant :: Text,
    estimated_price :: Price,
    discount :: Maybe Price,
    discounted_price :: Price,
    nearest_driver_distance :: DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Item where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
