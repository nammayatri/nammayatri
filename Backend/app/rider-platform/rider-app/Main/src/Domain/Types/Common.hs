{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Common where

import Data.Aeson
import Data.OpenApi
import qualified Domain.Types.FarePolicy.FareProductType as DTFP
import Kernel.Prelude

data FulfillmentType
  = DELIVERY
  | RIDE_OTP
  | RENTAL
  | INTER_CITY
  | AMBULANCE_FLOW
  deriving (Show, Eq, Generic, ToJSON, FromJSON, Read)

data PricingPolicy
  = EstimateBased
  | QuoteBased
  deriving stock (Eq, Show, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

fulfillmentToPricingPolicy :: FulfillmentType -> PricingPolicy
fulfillmentToPricingPolicy DELIVERY = EstimateBased
fulfillmentToPricingPolicy AMBULANCE_FLOW = EstimateBased
fulfillmentToPricingPolicy RIDE_OTP = QuoteBased
fulfillmentToPricingPolicy RENTAL = QuoteBased
fulfillmentToPricingPolicy INTER_CITY = QuoteBased

fulfillmentToFareProduct :: FulfillmentType -> DTFP.FareProductType
fulfillmentToFareProduct DELIVERY = DTFP.DRIVER_OFFER
fulfillmentToFareProduct ONE_WAY = DTFP.DRIVER_OFFER
fulfillmentToFareProduct AMBULANCE_FLOW = DTFP.AMBULANCE
fulfillmentToFareProduct RIDE_OTP = DTFP.ONE_WAY_SPECIAL_ZONE
fulfillmentToFareProduct RENTAL = DTFP.RENTAL
fulfillmentToFareProduct INTER_CITY = DTFP.INTER_CITY

fareProductToFulfillment :: DTFP.FareProductType -> FulfillmentType
fareProductToFulfillment DTFP.DRIVER_OFFER = DELIVERY
fareProductToFulfillment DTFP.DRIVER_OFFER = ONE_WAY
fareProductToFulfillment DTFP.AMBULANCE = AMBULANCE_FLOW
fareProductToFulfillment DTFP.ONE_WAY_SPECIAL_ZONE = RIDE_OTP
fareProductToFulfillment DTFP.RENTAL = RENTAL
fareProductToFulfillment DTFP.INTER_CITY = INTER_CITY

data UsageSafety = Safe | Unsafe

data TravelMode = Metro | Bus | Walk | Taxi
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show, Eq)
