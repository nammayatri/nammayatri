module Beckn.Types.Core.Taxi.Search.Fulfillment
  ( module Beckn.Types.Core.Taxi.Search.Fulfillment,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.Search.StartInfo
import Beckn.Types.Core.Taxi.Search.StopInfo
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
