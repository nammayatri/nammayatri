{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Search.Fulfillment
  ( module Beckn.Types.Core.Taxi.Search.Fulfillment,
    module Reexport,
  )
where

import Beckn.Types.Core.Taxi.Common.DecimalValue as Reexport
-- import Kernel.External.Types (Language)

import Beckn.Types.Core.Taxi.Common.Tags
import Beckn.Types.Core.Taxi.Search.StartInfo
import Beckn.Types.Core.Taxi.Search.StopInfo
import Data.Aeson
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import Kernel.Prelude
import Kernel.Utils.JSON
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

-- If end = Nothing, then bpp sends quotes only for RENTAL
-- If end is Just, then bpp sends quotes both for RENTAL and ONE_WAY
data FulfillmentInfo = FulfillmentInfo
  { start :: StartInfo,
    end :: Maybe StopInfo,
    tags :: Maybe TagGroups,
    customer :: Maybe Customer
  }
  deriving (Generic, Show)

instance ToSchema FulfillmentInfo where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance ToJSON FulfillmentInfo where
  toJSON = genericToJSON removeNullFields

instance FromJSON FulfillmentInfo where
  parseJSON = genericParseJSON removeNullFields

newtype Customer = Customer
  { person :: Person
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Customer where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype Person = Person
  { tags :: TagGroups
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Person where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

-- data PersonTag = PersonTag
--   {
--     code :: Text,
--     name :: Text,
--     list_1_code :: Maybe Text,
--     list_1_name :: Maybe Text,
--     list_1_value :: Maybe Text
--   }
--   deriving (Generic, Show)

-- instance ToJSON PersonTag where
--   toJSON = genericToJSON personTagJSONOptions

-- instance FromJSON PersonTag where
--   parseJSON = genericParseJSON personTagJSONOptions

-- instance ToSchema PersonTag where
--   declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions personTagJSONOptions

-- personTagJSONOptions :: Options
-- personTagJSONOptions =
--   defaultOptions
--     { fieldLabelModifier = \case
--         "code" -> "groups/1/descriptor/code"
--         "name" -> "groups/1/descriptor/name"
--         "list_1_code" -> "groups/1/list/1/descriptor/code"
--         "list_1_name" -> "groups/1/list/1/descriptor/name"
--         "list_1_value" -> "groups/1/list/1/value"
--         -- "display" -> "groups/1/display"
--         a -> a
--     }

-- data Tags = Tags
--   { --customer_language :: Maybe Language,
--     code :: Text,
--     name :: Text,
--     list_1_code :: Maybe Text,
--     list_1_name :: Maybe Text,
--     list_1_value :: Maybe Text,
--     list_2_code :: Maybe Text,
--     list_2_name :: Maybe Text,
--     list_2_value :: Maybe Text
--     -- display :: Bool,
--   }
--   deriving (Generic, Show)

-- instance ToJSON Tags where
--   toJSON = genericToJSON tagsJSONOptions

-- instance FromJSON Tags where
--   parseJSON = genericParseJSON tagsJSONOptions

-- instance ToSchema Tags where
--   declareNamedSchema = genericDeclareUnNamedSchema $ fromAesonOptions tagsJSONOptions

-- tagsJSONOptions :: Options
-- tagsJSONOptions =
--   defaultOptions
--     { fieldLabelModifier = \case
--         "code" -> "groups/1/descriptor/code"
--         "name" -> "groups/1/descriptor/name"
--         "list_1_code" -> "groups/1/list/1/descriptor/code"
--         "list_1_name" -> "groups/1/list/1/descriptor/name"
--         "list_1_value" -> "groups/1/list/1/value"
--         "list_2_code" -> "groups/1/list/2/descriptor/code"
--         "list_2_name" -> "groups/1/list/2/descriptor/name"
--         "list_2_value" -> "groups/1/list/2/value"
--         -- "display" -> "groups/1/display"
--         a -> a,
--       omitNothingFields = True
--     }
