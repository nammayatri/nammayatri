{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnCancel.Order (Order (..), BookingStatus (..), CancellationSource (..)) where

import Beckn.Types.Core.Taxi.OnCancel.Fulfillment (FulfillmentInfo)
import Data.Aeson
import Data.OpenApi
import EulerHS.Prelude hiding (id)
import Kernel.Utils.Schema (genericDeclareUnNamedSchema)

data Order = Order
  { id :: Text, -- BPP booking id
    status :: BookingStatus,
    cancellationSource :: CancellationSource,
    fulfillment :: Maybe FulfillmentInfo
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance ToSchema Order where
  declareNamedSchema = genericDeclareUnNamedSchema defaultSchemaOptions

data BookingStatus
  = NEW_BOOKING
  | TRIP_ASSIGNED
  | BOOKING_COMPLETED
  | BOOKING_CANCELLED
  deriving (Generic, Show)

instance ToJSON BookingStatus where
  toJSON = genericToJSON bookingStatusOptions

instance FromJSON BookingStatus where
  parseJSON = genericParseJSON bookingStatusOptions

instance ToSchema BookingStatus where
  declareNamedSchema = genericDeclareNamedSchema bookingStatusSchemaOptions

bookingStatusOptions :: Options
bookingStatusOptions =
  defaultOptions
    { constructorTagModifier = modifier
    }

bookingStatusSchemaOptions :: SchemaOptions
bookingStatusSchemaOptions =
  defaultSchemaOptions
    { constructorTagModifier = modifier
    }

modifier :: String -> String
modifier = \case
  "NEW_BOOKING" -> "NEW"
  "TRIP_ASSIGNED" -> "TRIP_ASSIGNED"
  "BOOKING_COMPLETED" -> "COMPLETED"
  "BOOKING_CANCELLED" -> "CANCELLED"
  x -> x

data CancellationSource
  = CANCELLED_BY_USER
  | CANCELLED_BY_DRIVER
  | CANCELLED_BY_MERCHANT
  | CANCELLED_BY_ALLOCATOR
  | CANCELLED_BY_APPLICATION
  deriving (Show, Eq, Ord, Read, Generic)

instance ToJSON CancellationSource where
  toJSON = genericToJSON cancellationSourceJSONOptions

instance FromJSON CancellationSource where
  parseJSON = genericParseJSON cancellationSourceJSONOptions

instance ToSchema CancellationSource where
  declareNamedSchema = genericDeclareNamedSchema cancellationSourceSchemaOptions

cancellationSourceJSONOptions :: Options
cancellationSourceJSONOptions =
  defaultOptions
    { constructorTagModifier = modifier
    }

cancellationSourceSchemaOptions :: SchemaOptions
cancellationSourceSchemaOptions =
  defaultSchemaOptions
    { constructorTagModifier = cancellationSourceSchemamodifier
    }

cancellationSourceSchemamodifier :: String -> String
cancellationSourceSchemamodifier = \case
  "CANCELLED_BY_USER" -> "ByUser"
  "CANCELLED_BY_DRIVER" -> "ByDriver"
  "CANCELLED_BY_MERCHANT" -> "ByMerchant"
  "CANCELLED_BY_ALLOCATOR" -> "ByAllocator"
  "CANCELLED_BY_APPLICATION" -> "ByApplication"
  _ -> error "CancellationReason parsing error"
