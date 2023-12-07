{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnSearch.Item
  ( module Beckn.Types.Core.Taxi.OnSearch.Item,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.Common.ItemCode as Reexport
-- import Kernel.External.Maps

import Beckn.Types.Core.Taxi.Common.Tags
import Data.Aeson
import Data.OpenApi (ToSchema (..), defaultSchemaOptions, fromAesonOptions)
import Kernel.Prelude
-- import Kernel.Types.Common

import Kernel.Utils.JSON (removeNullFields)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data ItemV2 = ItemV2
  { id :: Text,
    -- category_id :: FareProductType,
    -- id :: Text,
    fulfillment_id :: Text,
    -- offer_id :: Maybe Text,
    price :: ItemPrice,
    -- descriptor :: ItemDescriptor,
    -- quote_terms :: [Text],
    -- Only when FareProductType.ONE_WAY_TRIP
    tags :: Maybe [TagGroupV2]
    -- Only when FareProductType.RENTAL_TRIP
    -- base_distance :: Maybe Kilometers,
    -- base_duration :: Maybe Hours
    -- When we add some 3rd FareProductType, consider to make proper Item type without Maybes with custom To/FromJSON
  }
  deriving (Generic, Show)

instance ToJSON ItemV2 where
  toJSON = genericToJSON removeNullFields

instance FromJSON ItemV2 where
  parseJSON = genericParseJSON removeNullFields

instance ToSchema ItemV2 where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions removeNullFields

---------------- Code for backward compatibility : To be deprecated after v2.x release ----------------

data Item = Item
  { id :: Text,
    -- category_id :: FareProductType,
    -- id :: Text,
    fulfillment_id :: Text,
    -- offer_id :: Maybe Text,
    price :: ItemPrice,
    -- descriptor :: ItemDescriptor,
    -- quote_terms :: [Text],
    -- Only when FareProductType.ONE_WAY_TRIP
    tags :: Maybe TagGroups
    -- Only when FareProductType.RENTAL_TRIP
    -- base_distance :: Maybe Kilometers,
    -- base_duration :: Maybe Hours
    -- When we add some 3rd FareProductType, consider to make proper Item type without Maybes with custom To/FromJSON
  }
  deriving (Generic, Show)

instance ToJSON Item where
  toJSON = genericToJSON removeNullFields

instance FromJSON Item where
  parseJSON = genericParseJSON removeNullFields

instance ToSchema Item where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions removeNullFields

-- itemJSONOptions :: Options
-- itemJSONOptions =
--   defaultOptions
--     { fieldLabelModifier = \case
--         "base_distance" -> "./komn/rental/base_distance_km"
--         "base_duration" -> "./komn/rental/base_duration_hr"
--         "quote_terms" -> "./komn/quote_terms"
--         a -> a
--     }

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
    offered_value :: DecimalValue,
    minimum_value :: DecimalValue,
    maximum_value :: DecimalValue
    -- value_breakup :: [BreakupItem]
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema ItemPrice where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data BreakupItem = BreakupItem
  { title :: Text,
    price :: BreakupPrice
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema BreakupItem where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data BreakupPrice = BreakupPrice
  { currency :: Text,
    value :: DecimalValue
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema BreakupPrice where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

-- data ItemTags = ItemTags
--   { distance_to_nearest_driver :: Maybe DecimalValue,
--     night_shift_charge :: Maybe Money,
--     old_night_shift_charge :: Maybe DecimalValue, -- TODO: Doesn't make sense, to be removed
--     night_shift_start :: Maybe TimeOfDay,
--     night_shift_end :: Maybe TimeOfDay,
--     waiting_charge_per_min :: Maybe Money,
--     -- drivers_location :: [LatLong],
--     special_location_tag :: Maybe Text
--   }
--   deriving (Generic, Show)

data ItemTags = ItemTags
  { --customer_language :: Maybe Language,
    code_1 :: Maybe Text,
    name_1 :: Maybe Text,
    -- display_1 :: Bool,
    list_1_code :: Maybe Text,
    list_1_name :: Maybe Text,
    list_1_value :: Maybe Text,
    list_2_code :: Maybe Text,
    list_2_name :: Maybe Text,
    list_2_value :: Maybe Text,
    list_3_code :: Maybe Text,
    list_3_name :: Maybe Text,
    list_3_value :: Maybe Text,
    list_4_code :: Maybe Text,
    list_4_name :: Maybe Text,
    list_4_value :: Maybe Text,
    list_5_code :: Maybe Text,
    list_5_name :: Maybe Text,
    list_5_value :: Maybe Text,
    code_2 :: Maybe Text,
    name_2 :: Maybe Text,
    list_2_1_code :: Maybe Text,
    list_2_1_name :: Maybe Text,
    list_2_1_value :: Maybe Text,
    list_2_2_code :: Maybe Text,
    list_2_2_name :: Maybe Text,
    list_2_2_value :: Maybe Text
    -- display :: Bool,
  }
  deriving (Generic, Show)

instance ToJSON ItemTags where
  toJSON = genericToJSON itemTagsJSONOptions

instance FromJSON ItemTags where
  parseJSON = genericParseJSON itemTagsJSONOptions

instance ToSchema ItemTags where
  declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions itemTagsJSONOptions

-- itemTagsJSONOptions :: Options
-- itemTagsJSONOptions =
--   defaultOptions
--     { fieldLabelModifier = \case
--         "distance_to_nearest_driver" -> "./komn/distance_to_nearest_driver"
--         "night_shift_multiplier" -> "./komn/night_shift_multiplier"
--         "night_shift_start" -> "./komn/night_shift_start"
--         "night_shift_end" -> "./komn/night_shift_end"
--         "waiting_time_estimated_threshold" -> "./komn/waiting_time_estimated_threshold"
--         "waiting_charge_per_min" -> "./komn/waiting_charge_per_min"
--         a -> a
--     }

itemTagsJSONOptions :: Options
itemTagsJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "code_1" -> "groups/1/descriptor/code"
        "name_1" -> "groups/1/descriptor/name"
        "code_2" -> "groups/2/descriptor/code"
        "name_2" -> "groups/2/descriptor/name"
        "list_1_code" -> "groups/1/list/1/descriptor/code"
        "list_1_name" -> "groups/1/list/1/descriptor/name"
        "list_1_value" -> "groups/1/list/1/value"
        "list_2_code" -> "groups/1/list/2/descriptor/code"
        "list_2_name" -> "groups/1/list/2/descriptor/name"
        "list_2_value" -> "groups/1/list/2/value"
        "list_3_code" -> "groups/1/list/3/descriptor/code"
        "list_3_name" -> "groups/1/list/3/descriptor/name"
        "list_3_value" -> "groups/1/list/3/value"
        "list_4_code" -> "groups/1/list/4/descriptor/code"
        "list_4_name" -> "groups/1/list/4/descriptor/name"
        "list_4_value" -> "groups/1/list/4/value"
        "list_5_code" -> "groups/1/list/5/descriptor/code"
        "list_5_name" -> "groups/1/list/5/descriptor/name"
        "list_5_value" -> "groups/1/list/5/value"
        "list_2_1_code" -> "groups/2/list/1/descriptor/code"
        "list_2_1_name" -> "groups/2/list/1/descriptor/name"
        "list_2_1_value" -> "groups/2/list/1/value"
        "list_2_2_code" -> "groups/2/list/2/descriptor/code"
        "list_2_2_name" -> "groups/2/list/2/descriptor/name"
        "list_2_2_value" -> "groups/2/list/2/value"
        -- "display" -> "groups/1/display"
        a -> a
    }
