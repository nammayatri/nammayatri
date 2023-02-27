{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnInit.Order where

import Beckn.Types.Core.Taxi.OnInit.Fulfillment
import Beckn.Types.Core.Taxi.OnInit.OrderState
import Beckn.Types.Core.Taxi.OnInit.Payment
import Beckn.Types.Core.Taxi.OnInit.Quote
import Data.OpenApi
  ( ToSchema (..),
    defaultSchemaOptions,
  )
import EulerHS.Prelude hiding
  ( State,
    id,
    state,
  )
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Order = Order
  { id :: Text,
    state :: OrderState,
    items :: Maybe [OrderItem],
    quote :: Quote,
    payment :: Payment,
    fulfillment :: Maybe Fulfillment
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data OrderItem = OrderItem
  { quantity :: Quantity,
    id :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show)

data Quantity = Quantity
  { count :: Int32
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance ToSchema OrderItem where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

instance ToSchema Quantity where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions
