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
import Data.Aeson.Types (parseFail, typeMismatch)
import Kernel.Prelude
import Kernel.Utils.JSON
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Prelude (show)

-- #################################################################
-- This section contains type aliases for all Enums as per ONDC Spec
-- #################################################################

-- ONDC standard enums for ONDC:TRV10 domain

data VehicleCategory
  = -- ..fulfillments.vehicle.category
    AUTO_RICKSHAW
  | CAB
  | MOTORCYCLE
  | METRO
  | SUBWAY
  | BUS
  | AMBULANCE
  | TWO_WHEELER
  | TRUCK
  | BOAT
  | TOTO
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, Read, ToSchema)

$(mkHttpInstancesForEnum ''VehicleCategory)

data FulfillmentType
  = -- ..fulfillment.type
    DELIVERY
  | -- for on-us only
    RIDE_OTP
  | RENTAL
  | INTER_CITY
  | AMBULANCE_FLOW
  | METER_RIDE
  | SCHEDULED_TRIP
  deriving (Show, Eq, Generic, ToJSON, FromJSON, Read)

data StopType
  = -- ..fulfillments.stops.type
    START
  | END
  | INTERMEDIATE_STOP
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data AuthorizationType
  = -- ..fulfillments.stops.authorization.type
    OTP
  | QR -- not used in on-us
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data FulfillmentState
  = -- ..fulfillments.state.descriptor.code -- same enums are used in cancellation_terms
    RIDE_CANCELLED
  | RIDE_ENDED
  | RIDE_STARTED
  | SCHEDULED_RIDE_ASSIGNED
  | RIDE_ASSIGNED
  | RIDE_ENROUTE_PICKUP
  | RIDE_ARRIVED_PICKUP
  | NEW -- Custom type only used for on-us transaction
  | PAYMENT_COMPLETED -- Custom type only used for on-us transaction
  | EDIT_LOCATION -- Custom type only used for on-us transaction
  | ADD_STOP -- Custom type only used for on-us transaction
  | EDIT_STOP -- Custom type only used for on-us transaction
  | DRIVER_REACHED_DESTINATION
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PaymentStatus
  = -- ..fulfillments.payment.status
    PAID
  | NOT_PAID
  deriving (Eq, Generic)

instance Read PaymentStatus where
  readsPrec _ = \case
    "PAID" -> [(PAID, "")]
    "NOT-PAID" -> [(NOT_PAID, "")]
    _ -> []

instance Show PaymentStatus where
  show PAID = "PAID"
  show NOT_PAID = "NOT-PAID"

instance FromJSON PaymentStatus where
  parseJSON (String "PAID") = return PAID
  parseJSON (String "NOT-PAID") = return NOT_PAID
  parseJSON wrongVal = typeMismatch "Invalid PaymentStatus" wrongVal

instance ToJSON PaymentStatus where
  toJSON = genericToJSON constructorsWithHyphens

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
  deriving (Eq, Generic)

instance Show PaymentType where
  show PRE_ORDER = "PRE-ORDER"
  show ON_FULFILLMENT = "ON-FULFILLMENT"
  show POST_FULFILLMENT = "POST-FULFILLMENT"

instance ToJSON PaymentType where
  toJSON = genericToJSON constructorsWithHyphens

instance FromJSON PaymentType where
  parseJSON (String "PRE-ORDER") = return PRE_ORDER
  parseJSON (String "ON-FULFILLMENT") = return ON_FULFILLMENT
  parseJSON (String "POST-FULFILLMENT") = return POST_FULFILLMENT
  parseJSON wrongVal = typeMismatch "Invalid PaymentType" wrongVal

data OrderStatus
  = -- ..order.status
    SOFT_CANCEL
  | CONFIRM_CANCEL
  | ACTIVE
  | COMPLETE
  | CANCELLED
  | SOFT_UPDATE
  | CONFIRM_UPDATE
  deriving (Eq, Generic, Read, Show, FromJSON, ToJSON)

data QuoteBreakupTitle
  = -- ..quote.breakup.title
    BASE_FARE
  | DISTANCE_FARE
  | CANCELLATION_CHARGES
  | TOLL_CHARGES
  | PET_CHARGES
  | BUSINESS_DISCOUNT
  | PERSONAL_DISCOUNT
  | BUSINESS_DISCOUNT_PERCENTAGE
  | PERSONAL_DISCOUNT_PERCENTAGE
  | PRIORITY_CHARGES
  | STATE_ENTRY_PERMIT_CHARGES
  | CONGESTION_CHARGE
  | -- Custom Titles not in ONDC Spec
    SERVICE_CHARGE
  | DEAD_KILOMETER_FARE
  | DRIVER_SELECTED_FARE
  | CUSTOMER_SELECTED_FARE
  | TOTAL_FARE -- removed from init/on_init
  | WAITING_OR_PICKUP_CHARGES
  | PARKING_CHARGE
  | EXTRA_TIME_FARE
  | NIGHT_SHIFT_CHARGE
  | FIXED_GOVERNMENT_RATE
  | SGST
  | CGST
  | RIDE_VAT
  | TOLL_VAT
  | PLATFORM_FEE -- should this be in quote breakup?
  | TIME_BASED_FARE
  | DIST_BASED_FARE
  | EXTRA_DISTANCE_FARE
  | RIDE_DURATION_FARE
  | INSURANCE_CHARGES
  | CARD_CHARGES_ON_FARE
  | CARD_CHARGES_FIXED
  | SAFETY_PLUS_CHARGES
  | NO_CHARGES
  | RIDE_STOP_CHARGES
  | LUGGAGE_CHARGE
  | DRIVER_ALLOWANCE
  | RETURN_FEE
  | BOOTH_CHARGE
  | PER_STOP_CHARGES
  | NYREGULAR_SUBSCRIPTION_CHARGE
  | COMMISSION
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

data SafetyReasonCode
  = DEVIATION
  | RIDE_STOPPAGE
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show SafetyReasonCode where
  show DEVIATION = "DEVIATION"
  show RIDE_STOPPAGE = "RIDE_STOPPAGE"

instance Read SafetyReasonCode where
  readsPrec _ = \case
    "DEVIATION" -> [(DEVIATION, "")]
    "RIDE_STOPPAGE" -> [(RIDE_STOPPAGE, "")]
    _ -> []

data TLMethod
  = HttpGet
  | HttpPost
  | StripeSdk -- custom method for Stripe SDK
  deriving (Eq, Generic, Show, ToSchema)

instance FromJSON TLMethod where
  parseJSON (String "http/get") = pure HttpGet
  parseJSON (String "http/post") = pure HttpPost
  parseJSON (String "stripe/sdk") = pure StripeSdk
  parseJSON (String _) = parseFail "Invalid tl_method"
  parseJSON e = typeMismatch "tl_method string" e

instance ToJSON TLMethod where
  toJSON HttpGet = String "http/get"
  toJSON HttpPost = String "http/post"
  toJSON StripeSdk = String "stripe/sdk"

-- ##################################################################
-- PPF (Payment Protection Framework) Enums
-- ##################################################################

data PPFPaymentStatus
  = PPF_INITIATED
  | PPF_COLLECTED
  | PPF_HELD
  | PPF_RELEASED
  | PPF_SETTLED
  | PPF_REFUNDED
  | PPF_FAILED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''PPFPaymentStatus)

data PPFSettlementStatus
  = PPF_SETTLEMENT_PENDING
  | PPF_SETTLEMENT_IN_PROGRESS
  | PPF_SETTLEMENT_SETTLED
  | PPF_SETTLEMENT_FAILED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''PPFSettlementStatus)

data PPFReconStatus
  = RECON_ACCEPTED
  | RECON_DISPUTED
  | RECON_SETTLED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkHttpInstancesForEnum ''PPFReconStatus)
