module Beckn.Types.Core.Taxi.OnSearch.Fulfillment
  ( module Beckn.Types.Core.Taxi.OnSearch.Fulfillment,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.VehicleVariant as Reexport
import Beckn.Types.Core.Taxi.OnSearch.StartInfo
import Beckn.Types.Core.Taxi.OnSearch.StopInfo
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id)

data FulfillmentInfo = FulfillmentInfo
  { id :: Text,
    start :: StartInfo,
    end :: Maybe StopInfo,
    vehicle :: FulfillmentVehicle
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema FulfillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype FulfillmentVehicle = FulfillmentVehicle
  { category :: VehicleVariant
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema FulfillmentVehicle where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
