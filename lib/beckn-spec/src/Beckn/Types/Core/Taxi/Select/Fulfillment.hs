module Beckn.Types.Core.Taxi.Select.Fulfillment
  ( module Beckn.Types.Core.Taxi.Select.Fulfillment,
  )
where

import Beckn.Types.Core.Taxi.Select.StartInfo
import Beckn.Types.Core.Taxi.Select.StopInfo
import Data.Aeson
import Data.OpenApi (ToSchema (..), defaultSchemaOptions, fromAesonOptions)
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

-- If end = Nothing, then bpp sends quotes only for RENTAL
-- If end is Just, then bpp sends quotes both for RENTAL and ONE_WAY
data FulfillmentInfo = FulfillmentInfo
  { start :: StartInfo,
    tags :: Tags,
    end :: Maybe StopInfo
  }
  deriving (Generic, FromJSON, ToJSON, Show)

newtype Tags = Tags
  { auto_assign_enabled :: Bool
  }
  deriving (Generic, Show)

instance ToSchema FulfillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance ToJSON Tags where
  toJSON = genericToJSON tagsJSONOptions

instance FromJSON Tags where
  parseJSON = genericParseJSON tagsJSONOptions

instance ToSchema Tags where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions tagsJSONOptions

tagsJSONOptions :: Options
tagsJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "auto_assign_enabled" -> "./komn/auto_assign_enabled"
        a -> a
    }
