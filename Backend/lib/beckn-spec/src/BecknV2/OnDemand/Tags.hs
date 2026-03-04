{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module BecknV2.OnDemand.Tags where

import qualified BecknV2.OnDemand.Types as Spec
import Data.Char (toLower, toUpper)
import Data.Default.Class
import qualified Data.Map as M
import qualified Data.Map.Internal as MP
import qualified Data.Text as T
import Kernel.Prelude hiding (show)
import Text.Show

-- ##############################################################
-- This section contains type aliases for all TagGroups and Tags
-- ##############################################################

class CompleteTagGroup tagGroup where
  getTagGroupDescriptor :: Show tagGroup => tagGroup -> Spec.Descriptor
  getFullTagGroup :: tagGroup -> [Spec.Tag] -> Spec.TagGroup
  getTagGroupDisplay :: tagGroup -> Bool

class (Show tag, CompleteTagGroup (TagGroupF tag)) => CompleteTag tag where
  type TagGroupF tag
  getTagDescriptor :: tag -> Spec.Descriptor
  getFullTag :: tag -> Maybe Text -> Spec.Tag
  getTagDisplay :: tag -> Bool
  getTagGroup :: tag -> TagGroupF tag

type TagList = [(BecknTag, Maybe Text)]

data Taggings = Taggings
  { categoryTags :: TagList,
    contactTags :: TagList,
    fulfillmentTags :: TagList,
    intentTags :: TagList,
    itemTags :: TagList,
    personTags :: TagList,
    providerTags :: TagList,
    orderTags :: TagList,
    paymentTags :: TagList
  }

instance Default Taggings where
  def = Taggings [] [] [] [] [] [] [] [] []

data BecknTagGroup
  = -- ONDC standard tag groups for ONDC:TRV10 domain
    FARE_POLICY
  | INFO
  | BUYER_FINDER_FEES
  | SETTLEMENT_TERMS
  | ROUTE_INFO
  | -- ONDC 2.1.0 tag groups (subsume BUYER_FINDER_FEES + SETTLEMENT_TERMS)
    BAP_TERMS
  | BPP_TERMS
  | -- Custom tag groups
    REALLOCATION_INFO
  | SEARCH_REQUEST_INFO
  | FARE_PARAMETERS_IN_RATECARD_INFO
  | DRIVER_IDENTIFIER
  | CUSTOMER_INFO
  | PET_ORDER_INFO
  | BILLING_CATEGORY_INFO
  | ESTIMATIONS
  | CURRENT_LOCATION
  | DRIVER_DETAILS
  | DRIVER_ARRIVED_INFO
  | RIDE_DISTANCE_DETAILS
  | GENERAL_INFO -- TODO: How is this different from INFO?
  | AGENT_INFO
  | CUSTOMER_TIP_INFO
  | AUTO_ASSIGN_ENABLED
  | SAFETY_ALERT
  | RIDE_ODOMETER_DETAILS
  | TOLL_CONFIDENCE_INFO
  | VEHICLE_AGE_INFO
  | DRIVER_NEW_MESSAGE
  | PREVIOUS_CANCELLATION_REASONS
  | UPDATE_DETAILS
  | RATING_TAGS
  | FORWARD_BATCHING_REQUEST_INFO
  | VEHICLE_INFO
  | SETTLEMENT_DETAILS
  | DEVICE_ID_INFO
  | DELIVERY
  | DRIVER_REACHED_DESTINATION_INFO
  | ESTIMATED_END_TIME_RANGE
  | RIDE_DETAILS_INFO
  | SAFETY_PLUS_INFO
  | INSURANCE_INFO
  | BOOKING_INFO
  | EMAIL_DOMAIN_INFO
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

instance CompleteTagGroup BecknTagGroup where
  getFullTagGroup tagGroup tags = Spec.TagGroup (Just $ getTagGroupDescriptor tagGroup) (Just $ getTagGroupDisplay tagGroup) (if null tags then Nothing else Just tags)

  getTagGroupDisplay = \case
    VEHICLE_INFO -> True
    UPDATE_DETAILS -> True
    ESTIMATIONS -> True
    _ -> False

  -- getDescriptor :: tags -> (description, shortDescription)
  getTagGroupDescriptor tagGroup = uncurry (Spec.Descriptor . Just . T.pack $ show tagGroup) $ case tagGroup of
    ROUTE_INFO -> (Just "Route Information", Nothing)
    BUYER_FINDER_FEES -> (Just "Buyer Finder Fees Information", Nothing)
    SETTLEMENT_TERMS -> (Just "Settlement Terms Information", Nothing)
    BAP_TERMS -> (Just "BAP Terms of Engagement", Nothing)
    BPP_TERMS -> (Just "BPP Terms of Engagement", Nothing)
    REALLOCATION_INFO -> (Just "Reallocation Information", Nothing)
    FARE_PARAMETERS_IN_RATECARD_INFO -> (Just "Fare Parametes in RateCard information", Nothing)
    DELIVERY -> (Just "Delivery Information", Nothing)
    DRIVER_REACHED_DESTINATION_INFO -> (Just "Driver Reached Destination Information", Nothing)
    _ -> (Just $ convertToSentence tagGroup, Nothing) -- TODO: move all the tagGroups to this function and remove (_ -> case statement)

data EXTRA_PER_KM_STEP_FARE = EXTRA_PER_KM_STEP_FARE
  { startThreshold :: Int,
    endThreshold :: Maybe Int
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show EXTRA_PER_KM_STEP_FARE where
  show (EXTRA_PER_KM_STEP_FARE startDist (Just endDist)) = "EXTRA_PER_KM_STEP_FARE_" <> show startDist <> "_" <> show endDist
  show (EXTRA_PER_KM_STEP_FARE startDist Nothing) = "EXTRA_PER_KM_STEP_FARE_" <> show startDist <> "_Above"

data PLATFORM_FEE_STEP_FARE = PLATFORM_FEE_STEP_FARE
  { startThreshold :: Int,
    endThreshold :: Maybe Int
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show PLATFORM_FEE_STEP_FARE where
  show (PLATFORM_FEE_STEP_FARE startDist (Just endDist)) = "PLATFORM_FEE_STEP_FARE" <> show startDist <> "_" <> show endDist
  show (PLATFORM_FEE_STEP_FARE startDist Nothing) = "PLATFORM_FEE_STEP_FARE" <> show startDist <> "_Above"

data CONSTANT_PLATFORM_FEE_STEP_FARE = CONSTANT_PLATFORM_FEE_STEP_FARE
  { startThreshold :: Int,
    endThreshold :: Maybe Int
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show CONSTANT_PLATFORM_FEE_STEP_FARE where
  show (CONSTANT_PLATFORM_FEE_STEP_FARE startDist (Just endDist)) = "CONSTANT_PLATFORM_FEE_STEP_FARE" <> show startDist <> "_" <> show endDist
  show (CONSTANT_PLATFORM_FEE_STEP_FARE startDist Nothing) = "CONSTANT_PLATFORM_FEE_STEP_FARE" <> show startDist <> "_Above"

data PLATFORM_FEE_CGST_STEP_FARE = PLATFORM_FEE_CGST_STEP_FARE
  { startThreshold :: Int,
    endThreshold :: Maybe Int
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show PLATFORM_FEE_CGST_STEP_FARE where
  show (PLATFORM_FEE_CGST_STEP_FARE startDist (Just endDist)) = "PLATFORM_FEE_CGST_STEP_FARE" <> show startDist <> "_" <> show endDist
  show (PLATFORM_FEE_CGST_STEP_FARE startDist Nothing) = "PLATFORM_FEE_CGST_STEP_FARE" <> show startDist <> "_Above"

data PLATFORM_FEE_SGST_STEP_FARE = PLATFORM_FEE_SGST_STEP_FARE
  { startThreshold :: Int,
    endThreshold :: Maybe Int
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show PLATFORM_FEE_SGST_STEP_FARE where
  show (PLATFORM_FEE_SGST_STEP_FARE startDist (Just endDist)) = "PLATFORM_FEE_SGST_STEP_FARE" <> show startDist <> "_" <> show endDist
  show (PLATFORM_FEE_SGST_STEP_FARE startDist Nothing) = "PLATFORM_FEE_SGST_STEP_FARE" <> show startDist <> "_Above"

data PER_KM_STEP_FARE = PER_KM_STEP_FARE
  { startThreshold :: Int,
    endThreshold :: Maybe Int
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show PER_KM_STEP_FARE where
  show (PER_KM_STEP_FARE startDist (Just endDist)) = "PER_KM_STEP_FARE" <> show startDist <> "_" <> show endDist
  show (PER_KM_STEP_FARE startDist Nothing) = "PER_KM_STEP_FARE" <> show startDist <> "_Above"

data BASE_STEP_FARE = BASE_STEP_FARE
  { startThreshold :: Int,
    endThreshold :: Maybe Int
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show BASE_STEP_FARE where
  show (BASE_STEP_FARE startDist (Just endDist)) = "BASE_STEP_FARE" <> show startDist <> "_" <> show endDist
  show (BASE_STEP_FARE startDist Nothing) = "BASE_STEP_FARE" <> show startDist <> "_Above"

data BASE_STEP_DISTANCE = BASE_STEP_DISTANCE
  { startThreshold :: Int,
    endThreshold :: Maybe Int
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show BASE_STEP_DISTANCE where
  show (BASE_STEP_DISTANCE startDist (Just endDist)) = "BASE_STEP_DISTANCE" <> show startDist <> "_" <> show endDist
  show (BASE_STEP_DISTANCE startDist Nothing) = "BASE_STEP_DISTANCE" <> show startDist <> "_Above"

data NIGHT_SHIFT_STEP_PERCENTAGE = NIGHT_SHIFT_STEP_PERCENTAGE
  { startThreshold :: Int,
    endThreshold :: Maybe Int
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show NIGHT_SHIFT_STEP_PERCENTAGE where
  show (NIGHT_SHIFT_STEP_PERCENTAGE startDist (Just endDist)) = "NIGHT_SHIFT_STEP_PERCENTAGE" <> show startDist <> "_" <> show endDist
  show (NIGHT_SHIFT_STEP_PERCENTAGE startDist Nothing) = "NIGHT_SHIFT_STEP_PERCENTAGE" <> show startDist <> "_Above"

data CONSTANT_NIGHT_SHIFT_STEP_CHARGE = CONSTANT_NIGHT_SHIFT_STEP_CHARGE
  { startThreshold :: Int,
    endThreshold :: Maybe Int
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show CONSTANT_NIGHT_SHIFT_STEP_CHARGE where
  show (CONSTANT_NIGHT_SHIFT_STEP_CHARGE startDist (Just endDist)) = "CONSTANT_NIGHT_SHIFT_STEP_CHARGE" <> show startDist <> "_" <> show endDist
  show (CONSTANT_NIGHT_SHIFT_STEP_CHARGE startDist Nothing) = "CONSTANT_NIGHT_SHIFT_STEP_CHARGE" <> show startDist <> "_Above"

data WAITING_CHARGE_PER_MIN_STEP_FARE = WAITING_CHARGE_PER_MIN_STEP_FARE
  { startThreshold :: Int,
    endThreshold :: Maybe Int
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show WAITING_CHARGE_PER_MIN_STEP_FARE where
  show (WAITING_CHARGE_PER_MIN_STEP_FARE startDist (Just endDist)) = "WAITING_CHARGE_PER_MIN_STEP_FARE" <> show startDist <> "_" <> show endDist
  show (WAITING_CHARGE_PER_MIN_STEP_FARE startDist Nothing) = "WAITING_CHARGE_PER_MIN_STEP_FARE" <> show startDist <> "_Above"

data CONSTANT_WAITING_CHARGE_STEP_FARE = CONSTANT_WAITING_CHARGE_STEP_FARE
  { startThreshold :: Int,
    endThreshold :: Maybe Int
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show CONSTANT_WAITING_CHARGE_STEP_FARE where
  show (CONSTANT_WAITING_CHARGE_STEP_FARE startDist (Just endDist)) = "CONSTANT_WAITING_CHARGE_STEP_FARE" <> show startDist <> "_" <> show endDist
  show (CONSTANT_WAITING_CHARGE_STEP_FARE startDist Nothing) = "CONSTANT_WAITING_CHARGE_STEP_FARE" <> show startDist <> "_Above"

data FREE_WAITING_TIME_STEP_MINUTES = FREE_WAITING_TIME_STEP_MINUTES
  { startThreshold :: Int,
    endThreshold :: Maybe Int
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show FREE_WAITING_TIME_STEP_MINUTES where
  show (FREE_WAITING_TIME_STEP_MINUTES startDist (Just endDist)) = "FREE_WAITING_TIME_STEP_MINUTES" <> show startDist <> "_" <> show endDist
  show (FREE_WAITING_TIME_STEP_MINUTES startDist Nothing) = "FREE_WAITING_TIME_STEP_MINUTES" <> show startDist <> "_Above"

data PER_MINUTE_STEP_FARE = PER_MINUTE_STEP_FARE
  { startDurationThreshold :: Int,
    endDurationThreshold :: Maybe Int
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show PER_MINUTE_STEP_FARE where
  show (PER_MINUTE_STEP_FARE startDur (Just endDur)) = "PER_MINUTE_STEP_FARE_" <> show startDur <> "_" <> show endDur
  show (PER_MINUTE_STEP_FARE startDur Nothing) = "PER_MINUTE_STEP_FARE_" <> show startDur <> "_Above"

data DRIVER_EXTRA_FEE_BOUNDS_STEP_MIN_FEE = DRIVER_EXTRA_FEE_BOUNDS_STEP_MIN_FEE
  { startDistanceThreshold :: Int,
    endDistanceThreshold :: Maybe Int
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show DRIVER_EXTRA_FEE_BOUNDS_STEP_MIN_FEE where
  show (DRIVER_EXTRA_FEE_BOUNDS_STEP_MIN_FEE startDist (Just endDist)) = "DRIVER_EXTRA_FEE_BOUNDS_STEP_MIN_FEE_" <> show startDist <> "_" <> show endDist
  show (DRIVER_EXTRA_FEE_BOUNDS_STEP_MIN_FEE startDist Nothing) = "DRIVER_EXTRA_FEE_BOUNDS_STEP_MIN_FEE_" <> show startDist <> "_Above"

data DRIVER_EXTRA_FEE_BOUNDS_STEP_MAX_FEE = DRIVER_EXTRA_FEE_BOUNDS_STEP_MAX_FEE
  { startDistanceThreshold :: Int,
    endDistanceThreshold :: Maybe Int
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show DRIVER_EXTRA_FEE_BOUNDS_STEP_MAX_FEE where
  show (DRIVER_EXTRA_FEE_BOUNDS_STEP_MAX_FEE startDist (Just endDist)) = "DRIVER_EXTRA_FEE_BOUNDS_STEP_MAX_FEE_" <> show startDist <> "_" <> show endDist
  show (DRIVER_EXTRA_FEE_BOUNDS_STEP_MAX_FEE startDist Nothing) = "DRIVER_EXTRA_FEE_BOUNDS_STEP_MAX_FEE_" <> show startDist <> "_Above"

data BecknTag
  = -- ## Item tags ##
    -- FARE_POLICY
    MIN_FARE
  | MIN_FARE_DISTANCE_KM
  | PER_KM_CHARGE
  | DEAD_KILOMETER_FARE
  | WAITING_CHARGE_PER_MIN
  | WAITING_CHARGE_RATE_PER_MIN
  | NIGHT_CHARGE_MULTIPLIER
  | NIGHT_SHIFT_START_TIME
  | NIGHT_SHIFT_END_TIME
  | PER_STOP_CHARGES
  | PET_CHARGES
  | PRIORITY_CHARGES
  | BUSINESS_DISCOUNT
  | PERSONAL_DISCOUNT
  | PERSONAL_DISCOUNT_PERCENTAGE
  | BUSINESS_DISCOUNT_PERCENTAGE
  | NIGHT_SHIFT_START_TIME_IN_SECONDS
  | NIGHT_SHIFT_END_TIME_IN_SECONDS
  | NIGHT_SHIFT_CHARGE_PERCENTAGE
  | CONSTANT_NIGHT_SHIFT_CHARGE
  | RESTRICTED_PERSON
  | RESTRICTION_PROOF
  | DRIVER_MIN_EXTRA_FEE
  | DRIVER_MAX_EXTRA_FEE
  | EXTRA_PER_KM_FARE
  | WAITING_OR_PICKUP_CHARGES
  | CONSTANT_WAITING_CHARGE
  | FREE_WAITING_TIME_IN_MINUTES
  | SERVICE_CHARGE
  | PARKING_CHARGE
  | GOVERNMENT_CHARGE
  | BASE_DISTANCE
  | BASE_FARE
  | PROGRESSIVE_PLATFORM_CHARGE
  | CONSTANT_PLATFORM_CHARGE
  | PLATFORM_FEE_CGST
  | PLATFORM_FEE_SGST
  | TOLL_CHARGES
  | CANCELLATION_CHARGES
  | TIP_OPTIONS
  | CONGESTION_CHARGE_PERCENTAGE
  | UPDATED_ESTIMATED_DISTANCE
  | VEHICLE_AGE
  | -- INFO
    DISTANCE_TO_NEAREST_DRIVER_METER
  | DURATION_TO_NEAREST_DRIVER_MINUTES
  | SMART_TIP_SUGGESTION
  | QAR
  | SMART_TIP_REASON
  | ETA_TO_NEAREST_DRIVER_MIN
  | UPGRADE_TO_CAB
  | BUYER_FINDER_FEES_TYPE
  | BUYER_FINDER_FEES_PERCENTAGE
  | BUYER_FINDER_FEES_AMOUNT
  | LUGGAGE_CHARGE
  | DRIVER_ALLOWANCE
  | NUMBER_OF_LUGGAGE
  | -- ## Payment tags ##
    -- SETTLEMENT_TERMS
    SETTLEMENT_WINDOW
  | SETTLEMENT_BASIS
  | SETTLEMENT_TYPE
  | MANDATORY_ARBITRATION
  | COURT_JURISDICTION
  | DELAY_INTEREST
  | STATIC_TERMS
  | SETTLEMENT_AMOUNT
  | -- Stripe tags --
    STRIPE_TEST
  | PAYMENT_INSTRUMENT
  | -- ## Fulfillment tags ##
    -- ROUTE_INFO
    ENCODED_POLYLINE
  | WAYPOINTS
  | MULTIPLE_ROUTES
  | ROUTE_ID
  | ROUTE_DIRECTION
  | -- ###################
    -- Custom tags
    -- ###################
    -- Fare policy tags
    NIGHT_SHIFT_CHARGE
  | PER_HOUR_CHARGE
  | PER_MINUTE_CHARGE
  | UNPLANNED_PER_KM_CHARGE
  | PER_HOUR_DISTANCE_KM
  | PLANNED_PER_KM_CHARGE
  | PLANNED_PER_KM_CHARGE_ROUND_TRIP
  | PER_KM_RATE
  | PER_DAY_MAX_HOUR_ALLOWANCE
  | PER_DAY_MAX_ALLOWANCE_IN_MINS
  | RETURN_FEE
  | BOOTH_CHARGE
  | RETURN_FEE_PERCENTAGE
  | BOOTH_CHARGE_PERCENTAGE
  | -- | SETTLEMENT_DETAILS
    INSURANCE_CHARGE_PER_METER
  | INSURANCE_CHARGE_PER_MILE
  | INSURANCE_CHARGE_PER_KM
  | INSURANCE_CHARGE_PER_YARD
  | CARD_CHARGE_PERCENTAGE
  | FIXED_CARD_CHARGE
  | COMMISSION
  | -- Info tags
    SPECIAL_LOCATION_TAG
  | SPECIAL_LOCATION_NAME
  | IS_CUSTOMER_PREFFERED_SEARCH_ROUTE
  | IS_BLOCKED_SEARCH_ROUTE
  | TOLL_NAMES
  | -- Fulfillment tags
    DISTANCE_INFO_IN_M
  | DURATION_INFO_IN_S
  | RETURN_TIME
  | ROUND_TRIP
  | -- Reallocation tags
    IS_REALLOCATION_ENABLED
  | -- Meter ride tags
    IS_METER_RIDE_SEARCH
  | -- FareParametersInRatwCard tag
    FARE_PARAMETERS_IN_RATECARD
  | -- Driver identifier tags
    DRIVER_IDENTITY
  | -- Customer info tags
    CUSTOMER_LANGUAGE
  | CUSTOMER_DISABILITY
  | CUSTOMER_VEHICLE_CATEGORY
  | CUSTOMER_NAMMA_TAGS
  | DASHBOARD_USER
  | CUSTOMER_PHONE_NUMBER
  | USER_OS_TYPE
  | USER_OS_VERSION
  | USER_MODEL_NAME
  | USER_MANUFACTURER
  | USER_BUNDLE_VERSION
  | USER_SDK_VERSION
  | USER_BACKEND_APP_VERSION
  | RIDER_PREFERRED_OPTION
  | NIGHT_SAFETY_CHECK
  | ENABLE_FREQUENT_LOCATION_UPDATES
  | ENABLE_OTP_LESS_RIDE
  | -- Estimations tags
    MAX_ESTIMATED_DISTANCE
  | -- Location tags
    CURRENT_LOCATION_LAT
  | CURRENT_LOCATION_LON
  | -- Driver details tags
    REGISTERED_AT
  | RATING
  | IS_DRIVER_BIRTHDAY
  | IS_FREE_RIDE
  | DRIVER_TRACKING_URL
  | DRIVER_ACCOUNT_ID
  | DRIVER_ALTERNATE_NUMBER
  | IS_ALREADY_FAVOURITE
  | FAVOURITE_COUNT
  | -- Driver arrived info tags
    ARRIVAL_TIME
  | -- Ride distance details tags
    CHARGEABLE_DISTANCE
  | TRAVELED_DISTANCE
  | END_ODOMETER_READING
  | -- General info tags
    BPP_QUOTE_ID
  | -- Agent info tags
    DURATION_TO_PICKUP_IN_S
  | -- Customer tip info tags
    CUSTOMER_TIP
  | -- Auto assign enabled tags
    IS_AUTO_ASSIGN_ENABLED
  | -- Safety alert tags
    SAFETY_REASON_CODE
  | -- Ride odometer details tags
    START_ODOMETER_READING
  | -- Driver new message tags
    MESSAGE
  | -- Previous cancellation reasons tags
    CANCELLATION_REASON
  | -- Book Any estimates
    OTHER_SELECT_ESTIMATES
  | -- Forward batching request info tags
    PREVIOUS_RIDE_DROP_LOCATION_LAT
  | VEHICLE_ICON_URL
  | PREVIOUS_RIDE_DROP_LOCATION_LON
  | -- Toll related info tags
    TOLL_CONFIDENCE
  | -- Vehicle Air Conditioned info tag
    IS_AIR_CONDITIONED -- deprecated
  | IS_AIR_CONDITIONED_VEHICLE
  | IS_FORWARD_BATCH_ENABLED
  | -- rating tags
    RIDER_PHONE_NUMBER
  | SHOULD_FAVOURITE_DRIVER
  | DEVICE_ID_FLAG
  | TO_UPDATE_DEVICE_ID
  | MEDIA_FILE_PATH
  | RIDER_NAME
  | SENDER_NUMBER
  | SENDER_NAME
  | SENDER_LOCATION_INSTRUCTIONS
  | RECEIVER_NUMBER
  | RECEIVER_NAME
  | RECEIVER_LOCATION_INSTRUCTIONS
  | INITIATED_AS
  | DRIVER_REACHED_DESTINATION
  | ESTIMATED_END_TIME_RANGE_START
  | ESTIMATED_END_TIME_RANGE_END
  | PARCEL_IMAGE_UPLOADED
  | CUSTOMER_DISABILITY_DISABLE
  | IS_PET_RIDE
  | BILLING_CATEGORY
  | IS_VALID_RIDE
  | PARCEL_TYPE
  | PARCEL_QUANTITY
  | PREFER_SAFETY_PLUS
  | SAFETY_PLUS_CHARGES
  | NO_CHARGES
  | NYREGULAR_SUBSCRIPTION_CHARGE
  | IS_SAFETY_PLUS
  | IS_MULTIMODAL_SEARCH
  | IS_INSURED
  | INSURED_AMOUNT
  | RESERVED_RIDE_TAG
  | RESERVED_PRICING_TAG
  | FROM_SPECIAL_LOCATION_ID -- Fixed route: origin area ID
  | TO_SPECIAL_LOCATION_ID -- Fixed route: destination area ID
  | DISPLAY_BOOKING_ID -- Human-readable booking ID shared between BAP and BPP
  | EMAIL_DOMAIN
  | BUSINESS_EMAIL_DOMAIN
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance CompleteTag BecknTag where
  type TagGroupF BecknTag = BecknTagGroup

  getTagDisplay = \case
    IS_AIR_CONDITIONED -> True
    IS_AIR_CONDITIONED_VEHICLE -> True
    SPECIAL_LOCATION_TAG -> True
    SMART_TIP_REASON -> True
    MAX_ESTIMATED_DISTANCE -> True
    _ -> False

  -- getDescriptor :: tags -> (description, shortDescription)
  getTagDescriptor tag = uncurry (Spec.Descriptor . Just . T.pack $ show tag) $ case tag of
    NUMBER_OF_LUGGAGE -> (Just "Luggage Count", Nothing)
    DISTANCE_INFO_IN_M -> (Just "Distance Information In Meters", Nothing)
    DURATION_INFO_IN_S -> (Just "Duration Information In Seconds", Nothing)
    RETURN_TIME -> (Just "Return time in UTC", Nothing)
    ROUND_TRIP -> (Just "Round trip", Nothing)
    WAYPOINTS -> (Just "WAYPOINTS", Nothing)
    MULTIPLE_ROUTES -> (Just "Multiple Routes", Nothing)
    ROUTE_ID -> (Just "Route ID", Nothing)
    ROUTE_DIRECTION -> (Just "Route Direction", Nothing)
    DRIVER_IDENTITY -> (Just "Driver Identity", Nothing)
    BUYER_FINDER_FEES_PERCENTAGE -> (Just "Buyer Finder Fees", Nothing)
    SETTLEMENT_AMOUNT -> (Just "Settlement amount", Nothing)
    SETTLEMENT_WINDOW -> (Just "Settlement window", Nothing)
    STRIPE_TEST -> (Just "Stripe test", Nothing)
    DELAY_INTEREST -> (Just "Delay Interest", Nothing)
    SETTLEMENT_BASIS -> (Just "Settlement Basic", Nothing)
    MANDATORY_ARBITRATION -> (Just "Mandatory Arbitration", Nothing)
    COURT_JURISDICTION -> (Just "Court Jurisdiction", Nothing)
    STATIC_TERMS -> (Just "Static Terms", Nothing)
    SETTLEMENT_TYPE -> (Just "Settlement Type", Nothing)
    IS_REALLOCATION_ENABLED -> (Just "Is Reallocation Enabled", Nothing)
    IS_METER_RIDE_SEARCH -> (Just "Is Meter ride search", Nothing)
    IS_MULTIMODAL_SEARCH -> (Just "Is Multimodal Search", Nothing)
    RESERVED_RIDE_TAG -> (Just "Reserved Ride Tag", Nothing)
    RESERVED_PRICING_TAG -> (Just "Reserved Pricing Tag", Nothing)
    FARE_PARAMETERS_IN_RATECARD -> (Just "Fare Parametes in RateCard information", Nothing)
    SENDER_NUMBER -> (Just "Delivery Sender Number", Nothing)
    SENDER_NAME -> (Just "Delivery Sender Name", Nothing)
    SENDER_LOCATION_INSTRUCTIONS -> (Just "Delivery Sender Location Instructions", Nothing)
    RECEIVER_NUMBER -> (Just "Delivery Receiver Number", Nothing)
    RECEIVER_NAME -> (Just "Delivery Receiver Name", Nothing)
    RECEIVER_LOCATION_INSTRUCTIONS -> (Just "Delivery Receiver Location Instructions", Nothing)
    INITIATED_AS -> (Just "Delivery Initiated As", Nothing)
    DRIVER_REACHED_DESTINATION -> (Just "Destination Reached Time", Nothing)
    DISTANCE_TO_NEAREST_DRIVER_METER -> (Just "Distance To Nearest Driver In Meters", Nothing)
    ETA_TO_NEAREST_DRIVER_MIN -> (Just "Agent Duration to Pickup in Seconds", Nothing)
    SPECIAL_LOCATION_TAG -> (Just "Special Zone Tag", Nothing)
    UPGRADE_TO_CAB -> (Just "Request upgraded to cab", Nothing)
    PARCEL_TYPE -> (Just "Delivery Parcel Type", Nothing)
    PARCEL_QUANTITY -> (Just "Delivery Parcel Quantity", Nothing)
    SAFETY_PLUS_CHARGES -> (Just "safety plus charges", Nothing)
    IS_SAFETY_PLUS -> (Just "is safety plus", Nothing)
    NO_CHARGES -> (Just "no conditional charges", Nothing)
    IS_INSURED -> (Just "is insured", Nothing)
    INSURED_AMOUNT -> (Just "insured amount", Nothing)
    NYREGULAR_SUBSCRIPTION_CHARGE -> (Just "NYRegular subscription charge", Nothing)
    _ -> (Just $ convertToSentence tag, Nothing) -- TODO: move all the tags to this function and remove (_ -> case statement)

  getFullTag tag = Spec.Tag (Just $ getTagDescriptor tag) (Just $ getTagDisplay tag)

  getTagGroup = \case
    DISTANCE_INFO_IN_M -> ROUTE_INFO
    DURATION_INFO_IN_S -> ROUTE_INFO
    RETURN_TIME -> ROUTE_INFO
    ROUND_TRIP -> ROUTE_INFO
    WAYPOINTS -> ROUTE_INFO
    MULTIPLE_ROUTES -> ROUTE_INFO
    NUMBER_OF_LUGGAGE -> SEARCH_REQUEST_INFO
    DRIVER_IDENTITY -> DRIVER_IDENTIFIER
    BUYER_FINDER_FEES_PERCENTAGE -> BUYER_FINDER_FEES
    SETTLEMENT_AMOUNT -> SETTLEMENT_TERMS
    SETTLEMENT_WINDOW -> SETTLEMENT_TERMS
    STRIPE_TEST -> SETTLEMENT_TERMS
    PAYMENT_INSTRUMENT -> SETTLEMENT_TERMS
    DELAY_INTEREST -> SETTLEMENT_TERMS
    SETTLEMENT_BASIS -> SETTLEMENT_TERMS
    MANDATORY_ARBITRATION -> SETTLEMENT_TERMS
    COURT_JURISDICTION -> SETTLEMENT_TERMS
    STATIC_TERMS -> SETTLEMENT_TERMS
    SETTLEMENT_TYPE -> SETTLEMENT_DETAILS
    COMMISSION -> SETTLEMENT_DETAILS
    IS_REALLOCATION_ENABLED -> REALLOCATION_INFO
    IS_METER_RIDE_SEARCH -> SEARCH_REQUEST_INFO
    IS_MULTIMODAL_SEARCH -> SEARCH_REQUEST_INFO
    RESERVED_RIDE_TAG -> SEARCH_REQUEST_INFO
    RESERVED_PRICING_TAG -> SEARCH_REQUEST_INFO
    FROM_SPECIAL_LOCATION_ID -> SEARCH_REQUEST_INFO
    TO_SPECIAL_LOCATION_ID -> SEARCH_REQUEST_INFO
    FARE_PARAMETERS_IN_RATECARD -> FARE_PARAMETERS_IN_RATECARD_INFO
    CUSTOMER_LANGUAGE -> CUSTOMER_INFO
    CUSTOMER_VEHICLE_CATEGORY -> CUSTOMER_INFO
    CUSTOMER_PHONE_NUMBER -> CUSTOMER_INFO
    DASHBOARD_USER -> CUSTOMER_INFO
    CUSTOMER_DISABILITY -> CUSTOMER_INFO
    CUSTOMER_NAMMA_TAGS -> CUSTOMER_INFO
    USER_OS_TYPE -> CUSTOMER_INFO
    USER_OS_VERSION -> CUSTOMER_INFO
    USER_MODEL_NAME -> CUSTOMER_INFO
    USER_MANUFACTURER -> CUSTOMER_INFO
    USER_BUNDLE_VERSION -> CUSTOMER_INFO
    USER_SDK_VERSION -> CUSTOMER_INFO
    USER_BACKEND_APP_VERSION -> CUSTOMER_INFO
    RIDER_PREFERRED_OPTION -> CUSTOMER_INFO
    SENDER_NUMBER -> DELIVERY
    SENDER_NAME -> DELIVERY
    SENDER_LOCATION_INSTRUCTIONS -> DELIVERY
    RECEIVER_NUMBER -> DELIVERY
    RECEIVER_NAME -> DELIVERY
    RECEIVER_LOCATION_INSTRUCTIONS -> DELIVERY
    DRIVER_REACHED_DESTINATION -> DRIVER_REACHED_DESTINATION_INFO
    DISTANCE_TO_NEAREST_DRIVER_METER -> GENERAL_INFO
    ETA_TO_NEAREST_DRIVER_MIN -> GENERAL_INFO
    SPECIAL_LOCATION_TAG -> GENERAL_INFO
    SPECIAL_LOCATION_NAME -> GENERAL_INFO
    UPGRADE_TO_CAB -> GENERAL_INFO
    CUSTOMER_DISABILITY_DISABLE -> CUSTOMER_INFO
    IS_PET_RIDE -> PET_ORDER_INFO
    BILLING_CATEGORY -> BILLING_CATEGORY_INFO
    PARCEL_TYPE -> DELIVERY
    PARCEL_QUANTITY -> DELIVERY
    IS_SAFETY_PLUS -> DRIVER_DETAILS
    SAFETY_PLUS_CHARGES -> GENERAL_INFO
    IS_INSURED -> INSURANCE_INFO
    INSURED_AMOUNT -> INSURANCE_INFO
    NYREGULAR_SUBSCRIPTION_CHARGE -> GENERAL_INFO
    ROUTE_ID -> ROUTE_INFO
    ROUTE_DIRECTION -> ROUTE_INFO
    DISPLAY_BOOKING_ID -> BOOKING_INFO
    EMAIL_DOMAIN -> EMAIL_DOMAIN_INFO
    BUSINESS_EMAIL_DOMAIN -> EMAIL_DOMAIN_INFO
    -- Driver details tags
    REGISTERED_AT -> DRIVER_DETAILS
    RATING -> DRIVER_DETAILS
    IS_DRIVER_BIRTHDAY -> DRIVER_DETAILS
    IS_FREE_RIDE -> DRIVER_DETAILS
    DRIVER_TRACKING_URL -> DRIVER_DETAILS
    DRIVER_ACCOUNT_ID -> DRIVER_DETAILS
    DRIVER_ALTERNATE_NUMBER -> DRIVER_DETAILS
    IS_ALREADY_FAVOURITE -> DRIVER_DETAILS
    FAVOURITE_COUNT -> DRIVER_DETAILS
    -- Ride distance details tags
    CHARGEABLE_DISTANCE -> RIDE_DISTANCE_DETAILS
    TRAVELED_DISTANCE -> RIDE_DISTANCE_DETAILS
    END_ODOMETER_READING -> RIDE_DISTANCE_DETAILS
    -- Driver arrived info tags
    ARRIVAL_TIME -> DRIVER_ARRIVED_INFO
    -- Location tags
    CURRENT_LOCATION_LAT -> CURRENT_LOCATION
    CURRENT_LOCATION_LON -> CURRENT_LOCATION
    -- Odometer tags
    START_ODOMETER_READING -> RIDE_ODOMETER_DETAILS
    -- Toll confidence tags
    TOLL_CONFIDENCE -> TOLL_CONFIDENCE_INFO
    -- Vehicle age tags
    VEHICLE_AGE -> VEHICLE_AGE_INFO
    -- Vehicle info tags
    IS_AIR_CONDITIONED -> VEHICLE_INFO
    IS_AIR_CONDITIONED_VEHICLE -> VEHICLE_INFO
    VEHICLE_ICON_URL -> VEHICLE_INFO
    -- Estimated end time range tags
    ESTIMATED_END_TIME_RANGE_START -> ESTIMATED_END_TIME_RANGE
    ESTIMATED_END_TIME_RANGE_END -> ESTIMATED_END_TIME_RANGE
    -- Parcel tags
    PARCEL_IMAGE_UPLOADED -> GENERAL_INFO
    -- General info / Agent info tags
    BPP_QUOTE_ID -> GENERAL_INFO
    DURATION_TO_PICKUP_IN_S -> AGENT_INFO
    CUSTOMER_TIP -> CUSTOMER_TIP_INFO
    IS_AUTO_ASSIGN_ENABLED -> AUTO_ASSIGN_ENABLED
    SAFETY_REASON_CODE -> SAFETY_ALERT
    MESSAGE -> DRIVER_NEW_MESSAGE
    CANCELLATION_REASON -> PREVIOUS_CANCELLATION_REASONS
    OTHER_SELECT_ESTIMATES -> ESTIMATIONS
    MAX_ESTIMATED_DISTANCE -> ESTIMATIONS
    -- Forward batching tags
    PREVIOUS_RIDE_DROP_LOCATION_LAT -> FORWARD_BATCHING_REQUEST_INFO
    PREVIOUS_RIDE_DROP_LOCATION_LON -> FORWARD_BATCHING_REQUEST_INFO
    IS_FORWARD_BATCH_ENABLED -> FORWARD_BATCHING_REQUEST_INFO
    -- Rating tags
    RIDER_PHONE_NUMBER -> RATING_TAGS
    SHOULD_FAVOURITE_DRIVER -> RATING_TAGS
    RIDER_NAME -> RATING_TAGS
    MEDIA_FILE_PATH -> RATING_TAGS
    -- Device ID tags
    DEVICE_ID_FLAG -> DEVICE_ID_INFO
    TO_UPDATE_DEVICE_ID -> DEVICE_ID_INFO
    -- Ride details tags
    IS_VALID_RIDE -> RIDE_DETAILS_INFO
    -- Safety plus tags
    PREFER_SAFETY_PLUS -> SAFETY_PLUS_INFO
    -- Customer info tags (remaining)
    NIGHT_SAFETY_CHECK -> CUSTOMER_INFO
    ENABLE_FREQUENT_LOCATION_UPDATES -> CUSTOMER_INFO
    ENABLE_OTP_LESS_RIDE -> CUSTOMER_INFO
    INITIATED_AS -> DELIVERY
    -- Fare policy tags
    MIN_FARE -> FARE_POLICY
    MIN_FARE_DISTANCE_KM -> FARE_POLICY
    PER_KM_CHARGE -> FARE_POLICY
    DEAD_KILOMETER_FARE -> FARE_POLICY
    WAITING_CHARGE_PER_MIN -> FARE_POLICY
    WAITING_CHARGE_RATE_PER_MIN -> FARE_POLICY
    NIGHT_CHARGE_MULTIPLIER -> FARE_POLICY
    NIGHT_SHIFT_START_TIME -> FARE_POLICY
    NIGHT_SHIFT_END_TIME -> FARE_POLICY
    PER_STOP_CHARGES -> FARE_POLICY
    PET_CHARGES -> FARE_POLICY
    PRIORITY_CHARGES -> FARE_POLICY
    BUSINESS_DISCOUNT -> FARE_POLICY
    PERSONAL_DISCOUNT -> FARE_POLICY
    PERSONAL_DISCOUNT_PERCENTAGE -> FARE_POLICY
    BUSINESS_DISCOUNT_PERCENTAGE -> FARE_POLICY
    NIGHT_SHIFT_START_TIME_IN_SECONDS -> FARE_POLICY
    NIGHT_SHIFT_END_TIME_IN_SECONDS -> FARE_POLICY
    NIGHT_SHIFT_CHARGE_PERCENTAGE -> FARE_POLICY
    CONSTANT_NIGHT_SHIFT_CHARGE -> FARE_POLICY
    RESTRICTED_PERSON -> FARE_POLICY
    RESTRICTION_PROOF -> FARE_POLICY
    DRIVER_MIN_EXTRA_FEE -> FARE_POLICY
    DRIVER_MAX_EXTRA_FEE -> FARE_POLICY
    EXTRA_PER_KM_FARE -> FARE_POLICY
    WAITING_OR_PICKUP_CHARGES -> FARE_POLICY
    CONSTANT_WAITING_CHARGE -> FARE_POLICY
    FREE_WAITING_TIME_IN_MINUTES -> FARE_POLICY
    SERVICE_CHARGE -> FARE_POLICY
    PARKING_CHARGE -> FARE_POLICY
    GOVERNMENT_CHARGE -> FARE_POLICY
    BASE_DISTANCE -> FARE_POLICY
    BASE_FARE -> FARE_POLICY
    PROGRESSIVE_PLATFORM_CHARGE -> FARE_POLICY
    CONSTANT_PLATFORM_CHARGE -> FARE_POLICY
    PLATFORM_FEE_CGST -> FARE_POLICY
    PLATFORM_FEE_SGST -> FARE_POLICY
    TOLL_CHARGES -> FARE_POLICY
    CANCELLATION_CHARGES -> FARE_POLICY
    TIP_OPTIONS -> FARE_POLICY
    CONGESTION_CHARGE_PERCENTAGE -> FARE_POLICY
    UPDATED_ESTIMATED_DISTANCE -> UPDATE_DETAILS
    LUGGAGE_CHARGE -> FARE_POLICY
    DRIVER_ALLOWANCE -> FARE_POLICY
    NIGHT_SHIFT_CHARGE -> FARE_POLICY
    PER_HOUR_CHARGE -> FARE_POLICY
    PER_MINUTE_CHARGE -> FARE_POLICY
    UNPLANNED_PER_KM_CHARGE -> FARE_POLICY
    PER_HOUR_DISTANCE_KM -> FARE_POLICY
    PLANNED_PER_KM_CHARGE -> FARE_POLICY
    PLANNED_PER_KM_CHARGE_ROUND_TRIP -> FARE_POLICY
    PER_KM_RATE -> FARE_POLICY
    PER_DAY_MAX_HOUR_ALLOWANCE -> FARE_POLICY
    PER_DAY_MAX_ALLOWANCE_IN_MINS -> FARE_POLICY
    RETURN_FEE -> FARE_POLICY
    BOOTH_CHARGE -> FARE_POLICY
    RETURN_FEE_PERCENTAGE -> FARE_POLICY
    BOOTH_CHARGE_PERCENTAGE -> FARE_POLICY
    INSURANCE_CHARGE_PER_METER -> FARE_POLICY
    INSURANCE_CHARGE_PER_MILE -> FARE_POLICY
    INSURANCE_CHARGE_PER_KM -> FARE_POLICY
    INSURANCE_CHARGE_PER_YARD -> FARE_POLICY
    CARD_CHARGE_PERCENTAGE -> FARE_POLICY
    FIXED_CARD_CHARGE -> FARE_POLICY
    NO_CHARGES -> FARE_POLICY
    -- Info tags
    SMART_TIP_SUGGESTION -> INFO
    QAR -> INFO
    SMART_TIP_REASON -> INFO
    BUYER_FINDER_FEES_TYPE -> INFO
    BUYER_FINDER_FEES_AMOUNT -> INFO
    DURATION_TO_NEAREST_DRIVER_MINUTES -> INFO
    -- Fulfillment route tags
    ENCODED_POLYLINE -> ROUTE_INFO
    TOLL_NAMES -> ROUTE_INFO
    IS_CUSTOMER_PREFFERED_SEARCH_ROUTE -> ROUTE_INFO
    IS_BLOCKED_SEARCH_ROUTE -> ROUTE_INFO

convertToSentence :: Show a => a -> Text
convertToSentence = T.pack . toSentence . show
  where
    toSentence [] = []
    toSentence (x : xs) = toUpper x : map toLower' xs

    toLower' '_' = ' '
    toLower' c = toLower c

convertToTagGroup :: TagList -> Maybe [Spec.TagGroup]
convertToTagGroup = go . filter (isJust . snd)
  where
    go [] = Nothing
    go tagList = Just $ do
      let tagsWithGroup =
            foldl'
              ( \acc (tag, value) -> do
                  let tagGroup = getTagGroup tag
                  flip (M.insert tagGroup) acc $ case M.lookup tagGroup acc of
                    Nothing -> [getFullTag tag value]
                    Just tags -> getFullTag tag value : tags
              )
              mempty
              tagList
      M.elems $ MP.mapWithKey getFullTagGroup tagsWithGroup

-- ##############################################################
-- Tag Combinators
-- ##############################################################

-- | Always include a tag with the given value.
--
-- @REGISTERED_AT ~= show driver.createdAt@
(~=) :: BecknTag -> Text -> (BecknTag, Maybe Text)
tag ~= val = (tag, Just val)

infixl 7 ~=

-- | Include a tag only if the value is @Just@. @Nothing@ values are filtered out.
--
-- @RATING ~=? (show \<$\> driverStats.rating)@
(~=?) :: BecknTag -> Maybe Text -> (BecknTag, Maybe Text)
tag ~=? val = (tag, val)

infixl 7 ~=?

-- | Include a tag conditionally. If the condition is @False@, the tag is omitted.
--
-- @IS_FREE_RIDE ~=| (isFreeRide, show isFreeRide)@
(~=|) :: BecknTag -> (Bool, Text) -> (BecknTag, Maybe Text)
tag ~=| (cond, val) = (tag, if cond then Just val else Nothing)

infixl 7 ~=|

-- | Build @[Spec.TagGroup]@ from a flat list of @(BecknTag, Maybe Text)@ pairs.
-- Tags are automatically grouped by their 'getTagGroup' mapping.
-- @Nothing@-valued tags are filtered out. Returns @Nothing@ if all tags are @Nothing@.
buildTagGroups :: TagList -> Maybe [Spec.TagGroup]
buildTagGroups = convertToTagGroup

-- | Create a single 'Spec.Tag' from a tag enum and a value.
-- Useful when building tag lists for 'getFullTagGroup'.
mkTag :: BecknTag -> Maybe Text -> Spec.Tag
mkTag = getFullTag

-- | Create an optional tag. Returns @Nothing@ if the value is @Nothing@,
-- otherwise returns @Just@ the tag. Useful with @catMaybes@ when building
-- tag lists for 'getFullTagGroup'.
mkOptionalTag :: BecknTag -> Maybe Text -> Maybe Spec.Tag
mkOptionalTag _ Nothing = Nothing
mkOptionalTag tag val = Just $ getFullTag tag val

-- | Wrap a single optional @Show@-able value into a tag group.
-- Returns @Nothing@ if the input value is @Nothing@.
-- This eliminates the common pattern:
--
-- @
-- mkXxxTagGroup val' = val' >>= \\val -> buildTagGroups [TAG ~= show val]
-- @
mkSingleTagGroup :: Show a => BecknTag -> Maybe a -> Maybe [Spec.TagGroup]
mkSingleTagGroup tag = (>>= \v -> buildTagGroups [tag ~= T.pack (show v)])
