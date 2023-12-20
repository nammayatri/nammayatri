{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnSelect.Item
  ( module Beckn.Types.Core.Taxi.OnSelect.Item,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.Common.ItemCode as Reexport
import Beckn.Types.Core.Taxi.Common.Tags
import Beckn.Types.Core.Taxi.OnSelect.Price as Reexport
import Data.Aeson
import Data.OpenApi (ToSchema (..), defaultSchemaOptions, fromAesonOptions)
import Kernel.Prelude
-- import Kernel.Types.Common
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

-- id*	[...]
-- parent_item_id	Item/properties/id[...]
-- descriptor	Descriptor{...}
-- price	Price{...}
-- category_id	Category/properties/id[...]
-- fulfillment_id	Fulfillment/properties/id[...]
-- rating	Rating/properties/value[...]
-- location_id	Location/properties/id[...]
-- time	Time{...}
-- rateable	Rateable[...]
-- matched	[...]
-- related	[...]
-- recommended	[...]
-- tags	Tags{...}
-- quantity

data ItemV2 = ItemV2
  { id :: Text,
    -- category_id :: FareProductType,
    fulfillment_ids :: [Text],
    -- offer_id :: Maybe Text,
    price :: Price,
    -- descriptor :: ItemDescriptor,
    -- quote_terms :: [Text],
    -- Only when FareProductType.ONE_WAY_TRIP
    tags :: Maybe [TagGroupV2]
    -- Only when FareProductType.RENTAL_TRIP
    -- base_distance :: Maybe Kilometers,
    -- base_duration :: Maybe Hours,
    -- Only when FareProductType.DRIVER_OFFER
    -- driver_name :: Maybe Text,
    -- duration_to_pickup :: Maybe Int, -- Seconds?
    -- valid_till :: Maybe UTCTime
    -- rating :: Maybe Centesimal
    -- TODO consider to make proper Item type for different FareProductType without Maybes with custom To/FromJSON
  }
  deriving (Generic, Show)

instance ToJSON ItemV2 where
  toJSON = genericToJSON itemJSONOptions

instance FromJSON ItemV2 where
  parseJSON = genericParseJSON itemJSONOptions

instance ToSchema ItemV2 where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions itemJSONOptions

---------------- Code for backward compatibility : To be deprecated after v2.x release ----------------

data Item = Item
  { id :: Text,
    -- category_id :: FareProductType,
    fulfillment_id :: Text,
    -- offer_id :: Maybe Text,
    price :: Price,
    -- descriptor :: ItemDescriptor,
    -- quote_terms :: [Text],
    -- Only when FareProductType.ONE_WAY_TRIP
    tags :: Maybe TagGroups
    -- Only when FareProductType.RENTAL_TRIP
    -- base_distance :: Maybe Kilometers,
    -- base_duration :: Maybe Hours,
    -- Only when FareProductType.DRIVER_OFFER
    -- driver_name :: Maybe Text,
    -- duration_to_pickup :: Maybe Int, -- Seconds?
    -- valid_till :: Maybe UTCTime
    -- rating :: Maybe Centesimal
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

-- data Price = Price
--   { currency :: Text,
--     value :: DecimalValue,
--     offered_value :: DecimalValue
--   }
--   deriving (Generic, FromJSON, ToJSON, Show)

-- instance ToSchema Price where
--   declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data ItemTags = ItemTags
  { distance_to_nearest_driver :: DecimalValue,
    special_location_tag :: Maybe Text
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
