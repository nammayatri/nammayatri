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

import Beckn.Types.Core.Taxi.Confirm.Fulfillment
import Beckn.Types.Core.Taxi.Confirm.Payment
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (State, id, state)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Order = Order
  { id :: Text,
    fulfillment :: FulfillmentInfo,
    customer :: OrderCustomer,
    payment :: Payment
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data OrderCustomer = OrderCustomer
  { contact :: Contact,
    person :: Maybe OrderPerson
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema OrderCustomer where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype Contact = Contact
  { phone :: Phone
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Contact where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data Phone = Phone
  { country_code :: Text,
    number :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Phone where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

newtype OrderPerson = OrderPerson
  { name :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema OrderPerson where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
