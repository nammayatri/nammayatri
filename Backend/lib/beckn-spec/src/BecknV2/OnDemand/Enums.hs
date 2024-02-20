{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module BecknV2.OnDemand.Enums where

import Data.Aeson
import Data.Aeson.Types
import Kernel.Prelude
import Prelude (show)

-- #################################################################
-- This section contains type aliases for all Enums as per ONDC Spec
-- #################################################################

-- ONDC standard enums for ONDC:TRV10 domain

data VehicleCategory
  = -- ..fulfillments.vehicle.category
    AUTO_RICKSHAW
  | CAB
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data FulfillmentType
  = -- ..fulfillment.type
    DELIVERY
  | -- for on-us only
    RIDE_OTP
  | RENTAL
  | INTER_CITY
  deriving (Show, Eq, Generic, ToJSON, FromJSON, Read)

data StopType
  = -- ..fulfillments.stops.type
    START
  | END
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AuthorizationType
  = -- ..fulfillments.stops.authorization.type
    OTP
  | QR -- not used in on-us
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data FulfillmentState -- shrey00 : check if we have any other custom on-us only codes
  = -- ..fulfillments.state.descriptor.code -- same enums are used in cancellation_terms
    RIDE_CANCELLED
  | RIDE_ENDED
  | RIDE_STARTED
  | RIDE_ASSIGNED
  | RIDE_ENROUTE_PICKUP
  | RIDE_ARRIVED_PICKUP
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PaymentStatus
  = -- ..fulfillments.payment.status
    PAID
  | NOT_PAID
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PaymentCollectedBy
  = -- ..fulfillments.payment.collected.by
    BAP
  | BPP
  | SELLER
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PaymentType
  = -- ..fulfillments.payment.type -- we only support ON_FULFILLMENT for now
    PRE_ORDER
  | ON_FULFILLMENT
  | POST_FULFILLMENT
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data OrderStatus
  = -- ..order.status
    SOFT_CANCEL
  | CONFIRM_CANCEL
  | ACTIVE
  | COMPLETE
  | CANCELLED
  deriving (Show, Eq, Generic, ToJSON, FromJSON, Read)

data QuoteBreakupTitle
  = -- ..quote.breakup.title
    BASE_FARE
  | DISTANCE_FARE
  | CANCELLATION_CHARGES
  | -- Custom Titles not in ONDC Spec
    SERVICE_CHARGE
  | DEAD_KILOMETER_FARE
  | EXTRA_DISTANCE_FARE
  | DRIVER_SELECTED_FARE
  | CUSTOMER_SELECTED_FARE
  | TOTAL_FARE -- removed from init/on_init
  | WAITING_OR_PICKUP_CHARGES
  | EXTRA_TIME_FARE
  | NIGHT_SHIFT_CHARGE
  | FIXED_GOVERNMENT_RATE
  | SGST
  | CGST
  | PLATFORM_FEE -- should this be in quote breakup?
  | TIME_BASED_FARE
  | CUSTOMER_CANCELLATION_DUES
  | DIST_BASED_FARE
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CancellationReasonId
  = -- message.cancellation_reason_id -- sent by BAP in cancel
    DRIVER_NOT_MOVING --001
  | DRIVER_NOT_REACHABLE -- 002
  | DRIVER_ASKED_TO_CANCEL -- 003 -- Do we have this in frontend?
  | INCORRECT_PICKUP_LOCATION -- 004
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show CancellationReasonId where
  show DRIVER_NOT_MOVING = "001"
  show DRIVER_NOT_REACHABLE = "002"
  show DRIVER_ASKED_TO_CANCEL = "003"
  show INCORRECT_PICKUP_LOCATION = "004"

data CancellationReasonCode
  = -- message.order.cancellation.reason.descriptor.code -- sent by BPP in cancel
    NO_DRIVERS_AVAILABLE -- 011
  | COULD_NOT_FIND_CUSTOMER -- 012
  | RIDE_ACCEPTED_MISTAKENLY -- 013
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show CancellationReasonCode where
  show NO_DRIVERS_AVAILABLE = "011"
  show COULD_NOT_FIND_CUSTOMER = "012"
  show RIDE_ACCEPTED_MISTAKENLY = "013"

data CancelReqMessageCancellationReasonId
  = CANCELLED_BY_CUSTOMER -- 001
  | CANCELLED_BY_DRIVER -- 002
  deriving (Eq, Generic)

instance Show CancelReqMessageCancellationReasonId where
  show CANCELLED_BY_CUSTOMER = "001"
  show CANCELLED_BY_DRIVER = "002"

instance FromJSON CancelReqMessageCancellationReasonId where
  parseJSON (String "001") = return CANCELLED_BY_CUSTOMER
  parseJSON (String "002") = return CANCELLED_BY_DRIVER
  parseJSON wrongVal = typeMismatch "Invalid Cancellation Reason Id" wrongVal

instance ToJSON CancelReqMessageCancellationReasonId where
  toJSON CANCELLED_BY_CUSTOMER = String "001"
  toJSON CANCELLED_BY_DRIVER = String "002"

data CancellationSource
  = CONSUMER
  | PROVIDER
  deriving (Show, Eq, Generic, ToJSON, FromJSON, Read)
