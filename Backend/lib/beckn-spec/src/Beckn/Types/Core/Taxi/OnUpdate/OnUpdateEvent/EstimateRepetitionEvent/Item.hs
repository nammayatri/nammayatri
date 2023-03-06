{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.EstimateRepetitionEvent.Item
  ( module Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.EstimateRepetitionEvent.Item,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
import Beckn.Types.Core.Taxi.Common.ItemCode as Reexport
import Data.Aeson
import Data.OpenApi (ToSchema (..), defaultSchemaOptions, fromAesonOptions)
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Item = Item
  { category_id :: FareProductType,
    price :: ItemPrice,
    descriptor :: ItemDescriptor,
    quote_terms :: [Text],
    tags :: Maybe ItemTags
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

data ItemPrice = ItemPrice
  { currency :: Text,
    value :: DecimalValue,
    offered_value :: DecimalValue,
    minimum_value :: DecimalValue,
    maximum_value :: DecimalValue,
    value_breakup :: [BreakupItem]
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

data ItemTags = ItemTags
  { night_shift_multiplier :: Maybe DecimalValue,
    night_shift_start :: Maybe TimeOfDay,
    night_shift_end :: Maybe TimeOfDay,
    waiting_time_estimated_threshold :: Maybe Seconds,
    waiting_charge_per_min :: Maybe Money,
    drivers_location :: [LatLong]
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
        "night_shift_multiplier" -> "./komn/night_shift_multiplier"
        "night_shift_start" -> "./komn/night_shift_start"
        "night_shift_end" -> "./komn/night_shift_end"
        "waiting_time_estimated_threshold" -> "./komn/waiting_time_estimated_threshold"
        "waiting_charge_per_min" -> "./komn/waiting_charge_per_min"
        a -> a
    }
