{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module BecknV2.OnDemand.Tags where

-- ##############################################################################
-- This section contains type aliases for all standard Tags in ONDC:TRV10 domain
-- ##############################################################################

-- ## Item tags ##
-- Fare Policy Tag Group
-- Tag group
type TG_FARE_POLICY = "FARE_POLICY"

-- Tags
type T_MIN_FARE = "MIN_FARE"

type T_MIN_FARE_DISTANCE_KM = "MIN_FARE_DISTANCE_KM"

type T_PER_KM_CHARGE = "PER_KM_CHARGE"

type T_PICKUP_CHARGE = "PICKUP_CHARGE"

type T_WAITING_CHARGE_PER_MIN = "WAITING_CHARGE_PER_MIN"

type T_NIGHT_CHARGE_MULTIPLIER = "NIGHT_CHARGE_MULTIPLIER"

type T_NIGHT_SHIFT_START_TIME = "NIGHT_SHIFT_START_TIME"

type T_NIGHT_SHIFT_END_TIME = "NIGHT_SHIFT_END_TIME"

type T_RESTRICTED_PERSON = "RESTRICTED_PERSON"

type T_RESTRICTION_PROOF = "RESTRICTION_PROOF"

-- Info Tag Group
type TG_INFO = "INFO"

type T_DISTANCE_TO_NEAREST_DRIVER_METER = "DISTANCE_TO_NEAREST_DRIVER_METER"

type T_ETA_TO_NEAREST_DRIVER_MIN = "ETA_TO_NEAREST_DRIVER_MIN"

type TG_BUYER_FINDER_FEE = "BUYER_FINDER_FEE"

type T_BUYER_FINDER_FEES_TYPE = "BUYER_FINDER_FEES_TYPE"

type T_BUYER_FINDER_FEES_PERCENTAGE = "BUYER_FINDER_FEES_PERCENTAGE"

type T_BUYER_FINDER_FEES_AMOUNT = "BUYER_FINDER_FEES_AMOUNT"

-- ## Payment tags ##
-- Settlement Tag Group
type TG_SETTLEMENT_TERMS = "SETTLEMENT_TERMS"

type T_SETTLEMENT_WINDOW = "SETTLEMENT_WINDOW"

type T_SETTLEMENT_BASIS = "SETTLEMENT_BASIS"

type T_SETTLEMENT_TYPE = "SETTLEMENT_TYPE"

type T_MANDATORY_ARBITRATION = "MANDATORY_ARBITRATION"

type T_COURT_JURISDICTION = "COURT_JURISDICTION"

type T_DELAY_INTEREST = "DELAY_INTEREST"

type T_STATIC_TERMS = "STATIC_TERMS"

type T_SETTLEMENT_AMOUNT = "SETTLEMENT_AMOUNT"

-- ## Fulfillment tags ##
type TG_ROUTE_INFO = "ROUTE_INFO"

type T_ENCODED_POLYLINE = "ENCODED_POLYLINE"

type T_WAYPOINTS = "WAYPOINTS"

-- ############################################################################
-- Adding below custom tags
-- ############################################################################
-- ## Fulfillment tags ##
type T_DISTANCE_INFO_IN_M = "DISTANCE_INFO_IN_M"

type T_DURATION_INFO_IN_S = "DURATION_INFO_IN_S"

-- Reallocation Tag Group
type TG_REALLOCATION_INFO = "REALLOCATION_INFO"

type T_IS_REALLOCATION_ENABLED = "IS_REALLOCATION_ENABLED"

-- Customer info Tag Group
type TG_CUSTOMER_INFO = "CUSTOMER_INFO"

type T_CUSTOMER_LANGUAGE = "CUSTOMER_LANGUAGE"

type T_CUSTOMER_DISABILITY = "CUSTOMER_DISABILITY"

type T_CUSTOMER_PHONE_NUMBER = "CUSTOMER_PHONE_NUMBER"
