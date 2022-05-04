module Beckn.Types.Core.Taxi.OnSearch.Item
  ( module Beckn.Types.Core.Taxi.OnSearch.Item,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.Price as Reexport
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id)

-- TODO Fix OnSearch Catalog according to spec. We don't have actual Catalog design now
data Item = Item
  { id :: Text,
    category_id :: Text, -- ONE_WAY or RENTAL
    vehicle_variant :: Text,
    estimated_price :: Price,
    discount :: Maybe Price,
    discounted_price :: Price,
    nearest_driver_distance :: Maybe DecimalValue, -- only for ONE_WAY
    baseDistance :: Maybe Double, -- only for RENTAL
    baseDurationHr :: Maybe Int, -- only for RENTAL
    descriptions :: Maybe [Text] -- only for RENTAL
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Item where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
