module Beckn.Types.Core.Taxi.Init.Fulfillment
  ( module Beckn.Types.Core.Taxi.Init.Fulfillment,
  )
where

import Beckn.Types.Core.Taxi.Init.StartInfo
import Beckn.Types.Core.Taxi.Init.StopInfo
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id)

-- If end = Nothing, then bpp sends quotes only for RENTAL
-- If end is Just, then bpp sends quotes both for RENTAL and ONE_WAY
data FulfillmentInfo = FulfillmentInfo
  { start :: StartInfo,
    end :: Maybe StopInfo
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema FulfillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
