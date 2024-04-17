{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module BecknV2.OnDemand.Tags where

import Kernel.Prelude hiding (show)
import Text.Show

-- ##############################################################
-- This section contains type aliases for all TagGroups and Tags
-- ##############################################################

data TagGroup
  = -- ONDC standard tag groups for ONDC:TRV10 domain
    FARE_POLICY
  | INFO
  | BUYER_FINDER_FEES
  | SETTLEMENT_TERMS
  | ROUTE_INFO
  | -- Custom tag groups
    REALLOCATION_INFO
  | CUSTOMER_INFO
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
  | DRIVER_NEW_MESSAGE
  | PREVIOUS_CANCELLATION_REASONS
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data EXTRA_PER_KM_STEP_FARE = EXTRA_PER_KM_STEP_FARE
  { startDistanceThreshold :: Int,
    endDistanceThreshold :: Maybe Int
  }
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show EXTRA_PER_KM_STEP_FARE where
  show (EXTRA_PER_KM_STEP_FARE startDist (Just endDist)) = "EXTRA_PER_KM_STEP_FARE_" <> show startDist <> "_" <> show endDist
  show (EXTRA_PER_KM_STEP_FARE startDist Nothing) = "EXTRA_PER_KM_STEP_FARE_" <> show startDist <> "_Above"

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

data Tag
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
  | GOVERNMENT_CHARGE
  | BASE_DISTANCE
  | BASE_FARE
  | PROGRESSIVE_PLATFORM_CHARGE
  | CONSTANT_PLATFORM_CHARGE
  | PLATFORM_FEE_CGST
  | PLATFORM_FEE_SGST
  | TOLL_CHARGES
  | CONGESTION_CHARGE_PERCENTAGE
  | -- INFO
    DISTANCE_TO_NEAREST_DRIVER_METER
  | ETA_TO_NEAREST_DRIVER_MIN
  | BUYER_FINDER_FEES_TYPE
  | BUYER_FINDER_FEES_PERCENTAGE
  | BUYER_FINDER_FEES_AMOUNT
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
  | -- ## Fulfillment tags ##
    -- ROUTE_INFO
    ENCODED_POLYLINE
  | WAYPOINTS
  | MULTIPLE_ROUTES
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
  | SETTLEMENT_DETAILS
  | -- Info tags
    SPECIAL_LOCATION_TAG
  | IS_CUSTOMER_PREFFERED_SEARCH_ROUTE
  | IS_BLOCKED_SEARCH_ROUTE
  | -- Fulfillment tags
    DISTANCE_INFO_IN_M
  | DURATION_INFO_IN_S
  | -- Reallocation tags
    IS_REALLOCATION_ENABLED
  | -- Customer info tags
    CUSTOMER_LANGUAGE
  | CUSTOMER_DISABILITY
  | CUSTOMER_PHONE_NUMBER
  | NIGHT_SAFETY_CHECK
  | ENABLE_FREQUENT_LOCATION_UPDATES
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
    DEVIATION
  | -- Ride odometer details tags
    START_ODOMETER_READING
  | -- Driver new message tags
    MESSAGE
  | -- Previous cancellation reasons tags
    CANCELLATION_REASON
  | -- Book Any estimates
    OTHER_SELECT_ESTIMATES
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
