module Beckn.Types.Core.Taxi.Search.Intent
  ( module Beckn.Types.Core.Taxi.Search.Intent,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.Search.Fulfillment
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

newtype Intent = Intent
  { fulfillment :: FulfillmentInfo
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Intent where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
