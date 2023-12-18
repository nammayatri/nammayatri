{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Select.Order where

-- import Beckn.Types.Core.Taxi.Select.Quote

import Beckn.Types.Core.Taxi.Common.Tags
import Beckn.Types.Core.Taxi.OnSearch.Fulfillment
import Data.Aeson
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (State, id, state)
import Kernel.Utils.JSON
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data OrderV2 = OrderV2
  { items :: [OrderItemV2],
    fulfillments :: [FulfillmentInfoV2]
    -- quote :: Quote
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema OrderV2 where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data OrderItemV2 = OrderItemV2
  { id :: Text,
    price :: Price,
    tags :: Maybe [TagGroupV2]
  }
  deriving (Generic, Show)

instance ToSchema OrderItemV2 where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance FromJSON OrderItemV2 where
  parseJSON = genericParseJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

instance ToJSON OrderItemV2 where
  toJSON = genericToJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

data Price = Price
  { currency :: Text,
    value :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Price where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

---------------- Code for backward compatibility : To be deprecated after v2.x release ----------------

data Order = Order
  { items :: [OrderItem],
    fulfillment :: FulfillmentInfo
    -- quote :: Quote
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data OrderItem = OrderItem
  { id :: Text,
    price :: Price,
    tags :: Maybe TagGroups
  }
  deriving (Generic, Show)

instance ToSchema OrderItem where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance FromJSON OrderItem where
  parseJSON = genericParseJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

instance ToJSON OrderItem where
  toJSON = genericToJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}
