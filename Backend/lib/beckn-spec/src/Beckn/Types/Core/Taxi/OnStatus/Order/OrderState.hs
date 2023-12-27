{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.Types.Core.Taxi.OnStatus.Order.OrderState where

import Data.Aeson.Types
import Data.OpenApi
import Kernel.Prelude

-- In on_status API used order code instead of fulfillment code, because sometimes fulfillment is not present
-- Created separate types, because each order type should have single possible value for correct parsing
-- Constructors should be unique.

data NewBookingOrderCode = NEW_BOOKING
  deriving (Show, Generic, ToSchema)

data RideAssignedOrderCode = RIDE_ASSIGNED
  deriving (Show, Generic, ToSchema)

data RideStartedOrderCode = RIDE_STARTED
  deriving (Show, Generic, ToSchema)

data RideCompletedOrderCode = RIDE_COMPLETED
  deriving (Show, Generic, ToSchema)

data RideBookingCancelledOrderCode = RIDE_BOOKING_CANCELLED
  deriving (Show, Generic, ToSchema)

data RideBookingReallocationOrderCode = RIDE_BOOKING_REALLOCATION
  deriving (Show, Generic, ToSchema)

-- Generic instances for type with single value will not work
instance FromJSON NewBookingOrderCode where
  parseJSON (String "NEW_BOOKING") = pure NEW_BOOKING
  parseJSON (String _) = parseFail "Expected \"NEW_BOOKING\""
  parseJSON e = typeMismatch "String" e

instance ToJSON NewBookingOrderCode where
  toJSON = String . show

instance FromJSON RideAssignedOrderCode where
  parseJSON (String "RIDE_ASSIGNED") = pure RIDE_ASSIGNED
  parseJSON (String _) = parseFail "Expected \"RIDE_ASSIGNED\""
  parseJSON e = typeMismatch "String" e

instance ToJSON RideAssignedOrderCode where
  toJSON = String . show

instance FromJSON RideStartedOrderCode where
  parseJSON (String "RIDE_STARTED") = pure RIDE_STARTED
  parseJSON (String _) = parseFail "Expected \"RIDE_STARTED\""
  parseJSON e = typeMismatch "String" e

instance ToJSON RideStartedOrderCode where
  toJSON = String . show

instance FromJSON RideCompletedOrderCode where
  parseJSON (String "RIDE_COMPLETED") = pure RIDE_COMPLETED
  parseJSON (String _) = parseFail "Expected \"RIDE_COMPLETED\""
  parseJSON e = typeMismatch "String" e

instance ToJSON RideCompletedOrderCode where
  toJSON = String . show

instance FromJSON RideBookingCancelledOrderCode where
  parseJSON (String "RIDE_BOOKING_CANCELLED") = pure RIDE_BOOKING_CANCELLED
  parseJSON (String _) = parseFail "Expected \"RIDE_BOOKING_CANCELLED\""
  parseJSON e = typeMismatch "String" e

instance ToJSON RideBookingCancelledOrderCode where
  toJSON = String . show

instance FromJSON RideBookingReallocationOrderCode where
  parseJSON (String "RIDE_BOOKING_REALLOCATION") = pure RIDE_BOOKING_REALLOCATION
  parseJSON (String _) = parseFail "Expected \"RIDE_BOOKING_REALLOCATION\""
  parseJSON e = typeMismatch "String" e

instance ToJSON RideBookingReallocationOrderCode where
  toJSON = String . show
