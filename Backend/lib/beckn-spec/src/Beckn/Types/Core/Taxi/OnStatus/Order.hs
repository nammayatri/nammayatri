{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnStatus.Order where

import Beckn.Types.Core.Taxi.OnStatus.Fulfillment (FulfillmentInfo)
import Data.Aeson (Value (String))
import Data.OpenApi (ToSchema (..), defaultSchemaOptions)
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Order = Order
  { id :: Text, -- BPP booking id
    status :: BookingStatus,
    fulfillment :: FulfillmentInfo
  }
  deriving (Generic, FromJSON, ToJSON, Show)

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data BookingStatus
  = NEW_BOOKING
  | TRIP_ASSIGNED
  | BOOKING_COMPLETED
  | BOOKING_CANCELLED
  deriving (Generic, FromJSON, Show, ToSchema)

instance ToJSON BookingStatus where
  toJSON NEW_BOOKING = String "NEW"
  toJSON TRIP_ASSIGNED = String "TRIP_ASSIGNED"
  toJSON BOOKING_COMPLETED = String "COMPLETED"
  toJSON BOOKING_CANCELLED = String "CANCELLED"
