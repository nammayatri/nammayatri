module Beckn.Types.Core.Taxi.OnSelect.Item
  ( module Beckn.Types.Core.Taxi.OnSelect.Item,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Types.Common
import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.Common.ItemCode as Reexport
import Beckn.Utils.Schema (genericDeclareUnNamedSchema)
import Data.Aeson
import Data.OpenApi (ToSchema (..), defaultSchemaOptions, fromAesonOptions)

data Item = Item
  { id :: Text,
    category_id :: FareProductType,
    fulfillment_id :: Text,
    offer_id :: Maybe Text,
    price :: ItemPrice,
    descriptor :: ItemDescriptor,
    quote_terms :: [Text],
    -- Only when FareProductType.ONE_WAY_TRIP
    tags :: Maybe ItemTags,
    -- Only when FareProductType.RENTAL_TRIP
    base_distance :: Maybe Kilometers,
    base_duration :: Maybe Hours,
    -- Only when FareProductType.DRIVER_OFFER
    driver_name :: Maybe Text,
    duration_to_pickup :: Maybe Int, -- Seconds?
    valid_till :: Maybe UTCTime,
    rating :: Maybe Double
    -- TODO consider to make proper Item type for different FareProductType without Maybes with custom To/FromJSON
  }
  deriving (Generic, Show)

instance ToJSON Item where
  toJSON = genericToJSON itemJSONOptions

instance FromJSON Item where
  parseJSON = genericParseJSON itemJSONOptions

instance ToSchema Item where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions itemJSONOptions

itemJSONOptions :: Options
itemJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "base_distance" -> "./komn/rental/base_distance_km"
        "base_duration" -> "./komn/rental/base_duration_hr"
        "quote_terms" -> "./komn/quote_terms"
        a -> a
    }

data ItemDescriptor = ItemDescriptor
  { name :: Text,
    code :: ItemCode
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema ItemDescriptor where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data ItemPrice = ItemPrice
  { currency :: Text,
    value :: DecimalValue,
    offered_value :: DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema ItemPrice where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype ItemTags = ItemTags
  { distance_to_nearest_driver :: DecimalValue
  }
  deriving (Generic, Show)

instance ToJSON ItemTags where
  toJSON = genericToJSON itemTagsJSONOptions

instance FromJSON ItemTags where
  parseJSON = genericParseJSON itemTagsJSONOptions

instance ToSchema ItemTags where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions itemTagsJSONOptions

itemTagsJSONOptions :: Options
itemTagsJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "distance_to_nearest_driver" -> "./komn/distance_to_nearest_driver"
        a -> a
    }
