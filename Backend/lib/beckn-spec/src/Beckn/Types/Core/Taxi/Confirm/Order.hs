{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.Confirm.Order
  ( module Beckn.Types.Core.Taxi.Confirm.Order,
  )
where

import Beckn.Types.Core.Taxi.Common.Payment
import Beckn.Types.Core.Taxi.Common.Price
import Beckn.Types.Core.Taxi.Common.Provider
import Beckn.Types.Core.Taxi.Common.Quote
import Beckn.Types.Core.Taxi.Confirm.Fulfillment
import Data.Aeson
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (State, id, state)
import Kernel.Utils.JSON
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data OrderV2 = OrderV2
  { id :: Text,
    items :: [OrderItem],
    fulfillment :: FulfillmentInfo,
    quote :: Quote,
    payment :: PaymentV2,
    provider :: Maybe Provider
  }
  deriving (Generic, Show)

instance ToSchema OrderV2 where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance FromJSON OrderV2 where
  parseJSON = genericParseJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

instance ToJSON OrderV2 where
  toJSON = genericToJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

data OrderItem = OrderItem
  { id :: Text,
    price :: Maybe Price
  }
  deriving (Generic, Show)

instance ToSchema OrderItem where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance FromJSON OrderItem where
  parseJSON = genericParseJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

instance ToJSON OrderItem where
  toJSON = genericToJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

---------------- Code for backward compatibility : To be deprecated after v2.x release ----------------

data Order = Order
  { id :: Text,
    items :: [OrderItem],
    fulfillment :: FulfillmentInfo,
    quote :: Quote,
    payment :: Payment,
    provider :: Maybe Provider
  }
  deriving (Generic, Show)

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance FromJSON Order where
  parseJSON = genericParseJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}

instance ToJSON Order where
  toJSON = genericToJSON $ stripPrefixUnderscoreIfAny {omitNothingFields = True}
