module Beckn.Types.Core.Taxi.OnConfirm.Fulfillment
  ( module Beckn.Types.Core.Taxi.OnConfirm.Fulfillment,
  )
where

import Beckn.Types.Core.Taxi.OnConfirm.StartInfo
import Beckn.Types.Core.Taxi.OnConfirm.StopInfo
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import Kernel.Prelude
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

-- If end = Nothing, then bpp sends quotes only for RENTAL
-- If end is Just, then bpp sends quotes both for RENTAL and ONE_WAY
data FulfillmentInfo = FulfillmentInfo
  { state :: FulfillmentState,
    start :: StartInfo,
    end :: Maybe StopInfo
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema FulfillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype FulfillmentState = FulfillmentState
  { code :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema FulfillmentState where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
